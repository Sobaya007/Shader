import std : exp, writeln;
import sbylib;
import frag;
import rot;
import util;

mixin(Register!entryPoint);

@depends("root")
void entryPoint(ModuleContext context, Window window, ModuleContext[string] contexts) {
    auto tex = new FileTexture("resource/spheremap.jpg");
    context.pushResource(tex);
    auto hanger = Hanger.create(window, tex);
    context.pushResource(hanger);

    Rot q;
    float t = 0;
    
    with (context()) {
        when(Frame).then({
            with (hanger.fragmentUniform) {
                rot = q.toMatrix4;
                time = t;
            }
            t += 0.02;
        });

        when(KeyButton.KeyT.pressed.on(window)).then({
            t = 0;
        });
        handleContext(context, hanger);
        q.registerEvent(window);
    }
    contexts[getModuleName()] = context;
}

enum fragmentSource = q{
#version 450

precision highp float;
layout(location=0) in vec2 tc;
layout(location=0) out vec4 FragColor;

const float pi = 3.1415926535;
const float fov = 90 * pi / 180;
const float eps = 0.001;

layout(binding=0) uniform UniformData {
    float time;
    mat4 rot;
} uni;
layout(binding=1) uniform sampler2D mTexture;

float rand(vec3 p) {
  return fract(sin(dot(p, vec3(12.9898, 78.233, 114514.1919810))) * 43758.5453);
}

float interpolate(float x, float y, float t) {
  t = t * t * (3 - 2 * t);
  return x * (1-t) + y * t;
}

float irand(vec3 p) {
  vec3 i = floor(p);
  vec3 f = fract(p);
  float rand1 = rand(i);
  float rand2 = rand(i+vec3(1,0,0));
  float rand3 = rand(i+vec3(0,0,1));
  float rand4 = rand(i+vec3(1,0,1));
  float rand5 = rand(i+vec3(0,1,0));
  float rand6 = rand(i+vec3(1,1,0));
  float rand7 = rand(i+vec3(0,1,1));
  float rand8 = rand(i+vec3(1,1,1));
  float t1 = interpolate(rand1, rand2, f.x);
  float t2 = interpolate(rand3, rand4, f.x);
  float t3 = interpolate(rand5, rand6, f.x);
  float t4 = interpolate(rand7, rand8, f.x);
  float s1 = interpolate(t1, t2, f.z);
  float s2 = interpolate(t3, t4, f.z);
  return interpolate(s1, s2, f.y);
}

float noise(vec3 p) {
  float t = 0;
  for (int i = 0; i < 8; i++) {
    float freq = pow(2, float(i));
    float amp = pow(0.5, float(8-i));
    t += irand(p / freq) * amp;
  }
  return t;
}

float smin(float a, float b, float k) {
  float res = exp(-k * a) + exp(-k * b);
  return -log(res) / k;
}

vec3 translate(vec3 p, vec3 t) {
  return p - t;
}

vec3 rotate(vec3 p, vec3 n, float a) {
  a *= pi / 180;
  float c = cos(a);
  float s = -sin(a);
   mat3 m = mat3(
       n.x*n.x*(1-c)+c, n.x*n.y*(1-c)+n.z*s, n.x*n.z*(1-c)-n.y*s,
       n.y*n.x*(1-c)-n.z*s, n.y*n.y*(1-c)+c, n.y*n.z*(1-c)+n.x*s,
       n.z*n.x*(1-c)+n.y*s, n.z*n.y*(1-c)-n.x*s, n.z*n.z*(1-c)+c
       );
   return m * p;
}

vec3 scale(vec3 p, vec3 s) {
  return p / s;
}

float distHook(vec3 p) {
  p = rotate(p, vec3(0,1,0), uni.time * uni.time * 3);
  p = scale(p, vec3(0.8, 1,1));
  p = translate(p, vec3(0,1,0));
  float sub = length(translate(p, vec3(-0.7,-0.7,0))) - 0.8;
  vec2 q = vec2(length(p.xy) - 1, p.z);
  return max(length(q) - 0.1, -sub);
}

float distPoll(vec3 p) {
  p = translate(p, vec3(0,-1,0));
  p = scale(p, vec3(1,5,1));
  vec2 d = abs(vec2(length(p.xz), p.y)) - 0.15;
  return min(max(d.x, d.y), 0) + length(max(d,0));
}

float distMetalBall(vec3 p) {
  p = rotate(p, vec3(0,1,0), uni.time * uni.time * 3);
  p = translate(p, vec3(-0.80,0.9,0));
  return length(p) - 0.2;
}

float distMetalUnder(vec3 p) {
  p.x = abs(p.x);
  p = translate(p, vec3(4,-5,0));
  p = scale(p, vec3(1,10,1));
  vec2 d = abs(vec2(length(p.xz), p.y)) - 0.05;
  return min(max(d.x, d.y), 0) + length(max(d,0));
}

float distMetal(vec3 p) {
  float result = smin(distHook(p), distPoll(p), 17);
  result = smin(result, distMetalBall(p), 17);
  result = min(result, distMetalUnder(p));
  return result;
}

float distWood1(vec3 p) {
  p = scale(p, vec3(0.1+abs(p.y) * 0.5,1,1));
  p = translate(p, vec3(0,-1.5,0));
  return length(max(abs(p) - vec3(1.0,0.6,0.2), 0));
}

float distWood2(vec3 p) {
  p.x = abs(p.x);
  p = translate(p, vec3(0,-1.5,0));
  p = rotate(p, vec3(0,0,1) , -35);
  p = scale(p, vec3(1, p.x * p.x * 0.07 +2, p.x * p.x * p.x * 0.03 +1));
  vec3 s = vec3(0);
  vec3 e = vec3(5, 0, 0);
  vec3 ps = p - s;
  vec3 es = e - s;
  float h = clamp(dot(ps, es) / dot(es, es), 0, 1);
  float d = length(ps - es * h) - 0.2;
  float sub = p.y;
  return max(d, -sub);
}

float distWood(vec3 p) {
  return smin(distWood1(p), distWood2(p), 17);
}


float distUnder(vec3 p) {
  p = translate(p, vec3(0,-5.5,0));
  p = scale(p, vec3(20,1,1));
  vec2 d = abs(vec2(length(p.yz), p.x)) - 0.2;
  return min(max(d.x, d.y), 0) + length(max(d,0));
}

float dist(vec3 p) {
  p = translate(p, vec3(0,3,0));
  float result = min(distMetal(p), distWood(p));
  result = min(result, distUnder(p));
  return result;
  //return result-0.2-0.04*(sin(uni.time/3+p.x*p.x*0.3)+sin(20 * uni.time/3+p.y*p.y*0.20)+sin(uni.time/3+p.z*p.z*0.3));
}

int getNearestIndex(vec3 p) {
  p = translate(p, vec3(0,3,0));
  float dist = distMetal(p);
  int idx = 0;
  float d = distWood(p);
  if (d < dist) {
    dist = d;
    idx = 1;
  }
  d = distUnder(p);
  if (d < dist) {
    dist = d;
    idx = 2;
  }
  return idx;
}

vec3 getNormal(vec3 p) {
  return normalize(vec3(
        dist(p+vec3(eps,0,0)) - dist(p),
        dist(p+vec3(0,eps,0)) - dist(p),
        dist(p+vec3(0,0,eps)) - dist(p)
      ));
}


vec3 rayMarch(vec3 eye, vec3 ray) {
  vec3 current = eye;
  for (int i = 0; i < 200; i++) {
    float d = dist(current);
    current += ray * d;
    if (abs(d) < eps) {
      break;
    }
  }
  return current;
}

vec2 cubeMap(vec3 current, vec3 vec) {
  vec2 result;
  vec3 tmp;
  float minT = 1145141919;
  float size = 100;
  float t;

  //===========x
  t = (size - current.x) / vec.x;
  if (0 < t && t < minT) {
    tmp = current + t * vec;
    result = vec2(tmp.y, tmp.z);
  }

  t = -t;
  if (0 < t && t < minT) {
    tmp = current + t * vec;
    result = vec2(tmp.y, tmp.z);
  }

  //===============y
  t = (size - current.y) / vec.y;
  if (0 < t && t < minT) {
    tmp = current + t * vec;
    result = vec2(tmp.z, tmp.x);
  }

  t = -t;
  if (0 < t && t < minT) {
    tmp = current + t * vec;
    result = vec2(tmp.z, tmp.x);
  }

  //==============z
  t = (size - current.z) / vec.z;
  if (0 < t && t < minT) {
    tmp = current + t * vec;
    result = vec2(tmp.x, tmp.y);
  }

  t = -t;
  if (0 < t && t < minT) {
    tmp = current + t * vec;
    result = vec2(tmp.x, tmp.y);
  }
  return result  / (2 * size) + 1;
}


void main() {

  vec3 eye = vec3(0,0,5);
  eye = (uni.rot * vec4(eye,0)).xyz;
  vec3 ray = vec3(tc * 2 * tan(fov/2), -1);
  ray = (uni.rot * vec4(ray,0)).xyz;
  ray = normalize(ray);
  vec3 current = rayMarch(eye, ray);

  vec3 lightPos = vec3(1,1,-1) * 10;

  if (abs(dist(current)) < eps) {
    vec3 n = getNormal(current);
    vec3 eyeVec = normalize(eye - current);
    vec3 lightVec = normalize(lightPos - current);
    vec3 refVec = reflect(-lightVec, n);
    float diffuse = max(0,dot(eyeVec, n));
    float spec = max(0,dot(eyeVec, refVec));
    switch (getNearestIndex(current)) {
      case 0:
        vec3 reflectColor = texture(mTexture, cubeMap(current, refVec)).rgb;
        FragColor.rgb = reflectColor * 0.2 + vec3(0.5,0.5,0.5) * 0.8;
        FragColor.rgb *= diffuse;
        FragColor.rgb += pow(spec, 5);
        break;
      case 1:
        vec3 dir = vec3(0.01,0.2,0.5);
        float t = noise(current * noise(current * 100) * 3000 * dir);
        vec3 lightColor = vec3(213,136,76) / 255;
        vec3 darkColor = vec3(lightColor.rgb / 4);
        FragColor.rgb = mix(lightColor, darkColor, t) * diffuse;
        FragColor.rgb += darkColor * pow(spec, 30) * (1-t);
        break;
      case 2:
        FragColor.rgb = vec3(0);
        break;
      default:
        FragColor.rgb = vec3(0,0,1);
        break;
    }
  } else {
    FragColor.rgb = vec3(0.25);
  }
  FragColor.a = 1;
}
};

struct Uniform {
align(16):
    float time;
    mat4 rot;
}

alias Hanger = FragmentCanvas!(fragmentSource, Uniform, 1);
