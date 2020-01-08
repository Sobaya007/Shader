import std : exp, writeln;
import sbylib;
import frag;
import rot;
import util;

mixin(Register!entryPoint);

@depends("root")
void entryPoint(ModuleContext context, Window window, ModuleContext[string] contexts) {
    auto tex = new FileTexture("resource/sobaya.png");
    context.pushResource(tex);
    auto sense = Sense.create(window, tex);
    context.pushResource(sense);

    Rot q;
    float t = 0;
    
    with (context()) {
        when(Frame).then({
            with (sense.fragmentUniform) {
                rot = q.toMatrix4;
                time = t;
            }
            t += 0.2;
        });

        when(KeyButton.KeyT.pressed.on(window)).then({
            t = 0;
        });
        handleContext(context, sense);
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
const vec3 lightPos = vec3(0,3.6,0);
const vec3 center = vec3(0,3.6,0);
const vec3 ambientColor = vec3(0.04);

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

const float boneNum = 11;
const float timeCoef = 0.1;

float bone(vec3 p) {
  const vec3 s = vec3(0);
  const vec3 e = vec3(0,20,0);
  const vec3 v = e - s;
  const float r = 0.3;
  float senseAngle = 120 * (1 - exp(-uni.time * timeCoef));
  float deltaSenseAngle = senseAngle / boneNum * pi / 180;

  p = translate(p, vec3(0,-10,0));
  float a = atan(p.x, p.y);
  if (abs(a) > senseAngle / 180 * pi / 2) return 1;
  p.z += floor(a / deltaSenseAngle + 0.5) * 0.5;
  a = mod(a + deltaSenseAngle/2, deltaSenseAngle) - deltaSenseAngle / 2;
  p.xy = vec2(sin(a), cos(a)) * length(p.xy);

  vec3 ps = p - s;
  float h = clamp(dot(ps, v) / dot(v,v), 0, 1);
  float result = length(ps - v * h) - r;
  result = max(result, p.z - 0.2);
  result = max(result, -p.z - 0.2);
  return result;
}

float paper(vec3 p) {
  const float longest = 21;
  const float shortest = 10;
  const float deltaPaperAngle = 0.06;
  float senseAngle = 120 * (1 - exp(-uni.time * timeCoef));
  float deltaSenseAngle = senseAngle / boneNum * pi / 180;

  p = translate(p, vec3(0,-10,0));
  float a = atan(p.x, p.y);
  if (abs(a) > senseAngle / 180 * pi / 2) return 1;
  p.z += floor(a / deltaSenseAngle + 0.5) * 0.5 + 0.5;
  a = mod(a + deltaSenseAngle/2, deltaSenseAngle) - deltaSenseAngle / 2;
  p.xy = vec2(sin(a), cos(a)) * length(p.xy);
  p = rotate(p, vec3(0,1,0), -20);

  //angle
  a = clamp(a,-deltaPaperAngle,+deltaPaperAngle);
  vec3 v = vec3(sin(a), cos(a), 0);

  //length
  float len = clamp(length(p), shortest, longest);

  //point
  vec3 point = v * len;
  p.z/= 1.5;

  return length(p - point) - 0.05;
}

float paper2(vec3 p) {
  const float longest = 21;
  const float shortest = 10;
  const float deltaPaperAngle2 = 0.2;
  float senseAngle = 120 * (1 - exp(-uni.time * timeCoef));
  float deltaSenseAngle = senseAngle / boneNum * pi / 180;

  p = translate(p, vec3(0,-10,0));
  float a = atan(p.x, p.y);
  if (abs(a) > senseAngle / 180 * pi / 2) return 1;
  p.z += floor(a / deltaSenseAngle) * 0.5 + 0.75;
  a = mod(a, deltaSenseAngle) - deltaSenseAngle / 2;
  p.xy = vec2(sin(a), cos(a)) * length(p.xy);
  p = rotate(p, vec3(0,1,0), +55);

  //angle
  a = clamp(a,-deltaPaperAngle2,+deltaPaperAngle2);
  vec3 v = vec3(sin(a), cos(a), 0);

  //length
  float len = clamp(length(p), shortest, longest);

  //point
  vec3 point = v * len;
  p.z/= 1.5;

  return length(p - point) - 0.2;
}

float dist(vec3 p) {
  float result = 114514;
  result = min(result, paper(p));
  result = min(result, paper2(p));
  result = min(result, bone(p));
  return result;
}

int getNearestIndex(vec3 p) {
  float minDist = bone(p);
  int idx = 0;
  float d = min(paper(p), paper2(p));
  if (d < minDist) {
    minDist = d;
    idx = 1;
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
    minT = t;
  }

  t = (-size - current.x) / vec.x;
  if (0 < t && t < minT) {
    tmp = current + t * vec;
    result = vec2(tmp.y, tmp.z);
    minT = t;
  }

  //===============y
  t = (size - current.y) / vec.y;
  if (0 < t && t < minT) {
    tmp = current + t * vec;
    result = vec2(tmp.z, tmp.x);
    minT = t;
  }

  t = (-size - current.y) / vec.y;
  if (0 < t && t < minT) {
    tmp = current + t * vec;
    result = vec2(tmp.z, tmp.x);
    minT = t;
  }

  //==============z
  t = (size - current.z) / vec.z;
  if (0 < t && t < minT) {
    tmp = current + t * vec;
    result = vec2(tmp.x, tmp.y);
    minT = t;
  }

  t = (-size - current.z) / vec.z;
  if (0 < t && t < minT) {
    tmp = current + t * vec;
    result = vec2(tmp.x, tmp.y);
    minT = t;
  }
  return result  / (2 * size) + 1;
}

float beckmann(float m, float c) {
  return exp(-(1 - c * c) / (m * m * c * c)) / (4 * m * m * c * c * c * c);
}

float fresnel(float n, float c) {
  float g = sqrt(n * n + c * c - 1);
  float a = (g-c) / (g+c);
  float b = (c * (g+c) - 1) / (c * (g-c) + 1);
  return a * a * (1 + b * b) / 2;
}

float cookTrance(vec3 N, vec3 lightPos, vec3 eye, vec3 current, float m, float n) {
  vec3 L = normalize(lightPos - current);
  vec3 V = normalize(eye - current);
  vec3 H = normalize(L + V);

  float NV = dot(N,V);
  float NH = dot(N,H);
  float VH = dot(V,H);
  float NL = dot(N,L);
  
  float D = beckmann(m, NH);
  float G = min(1, 2 * NH / VH * min(NV, NL));
  float F = fresnel(n, dot(L,H));

  return max(0, F * D * G / NV);
}

void main() {
  vec2 uv = tc;
  vec3 eye = vec3(0,0,-15);
  eye = (uni.rot * vec4(eye,0)).xyz;
  vec3 ray = vec3(uv * 2 * tan(fov/2), 1);
  ray = (uni.rot * vec4(ray,0)).xyz;
  ray = normalize(ray);
  vec3 current = rayMarch(eye, ray);

  if (abs(dist(current)) < eps) {
    vec3 n = getNormal(current);
    vec3 eyeVec = normalize(eye - current);
    float diffuse = max(0,dot(eyeVec, n));
    float spec = cookTrance(n, lightPos, eye, current, 3.5, 2);
    float light = 0;
    switch (getNearestIndex(current)) {
      case 1:
        current.y += 10;
        float senseAngle = 120 * (1 - exp(-uni.time * timeCoef));
        float angle = atan(current.x, current.y);
        angle /= pi * 2 / 3;
        angle *= 120 / senseAngle;
        angle += 0.5;
        angle *= 3;
        float r = length(current);
        r -= 10;
        r /= 11;
        r *= 1.2;
        r = min(r, 1);
        FragColor.rgb = texture(mTexture, vec2(angle, r)).rgb * diffuse;
        break;
      case 0:
        vec3 dir = vec3(0.01,0.2,0.5);
        float t = noise(current * noise(current * 100) * 3000 * dir);
        vec3 lightColor = vec3(213,136,76) / 255;
        vec3 darkColor = vec3(lightColor.rgb / 4);
        FragColor.rgb = mix(lightColor, darkColor, t) * diffuse;
        FragColor.rgb += darkColor * pow(spec, 30) * (1-t);
        break;
      default:
        FragColor.rgb = vec3(0,0,1);
        break;
    }
  } else {
    FragColor.rgb = vec3(0.6);
  }
  FragColor.a = 1;
}
};

struct Uniform {
align(16):
    float time;
    mat4 rot;
}

alias Sense = FragmentCanvas!(fragmentSource, Uniform, 1);
