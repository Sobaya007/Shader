import std : exp, writeln;
import sbylib;
import frag;
import rot;

mixin(Register!entryPoint);
void entryPoint(Project proj, ModuleContext context, Window window) {
    auto tex = new FileTexture("resource/spheremap.jpg");
    context.pushResource(tex);
    auto bolt = Basami.create(window, tex);
    context.pushResource(bolt);

    Rot q;
    float t = 0;
    vec3 c = vec3(0.9,0.8,0.4);
    
    with (context()) {
        when(Frame).then({
            with (bolt.fragmentUniform) {
                rot = q.toMatrix4;
                time = t;
            }
            t += 0.2;
        });

        when(KeyButton.KeyR.pressed.on(window)).then({
            proj.reloadAll();
        });
        when(KeyButton.KeyT.pressed.on(window)).then({
            t = 0;
        });
        q.registerEvent(window);
    }
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

float poll(vec3 p) {
  p = scale(p, vec3(1.2, 1.5, 1));
  p = translate(p, vec3(-0.5,0.3,0));
  vec2 d = abs(vec2(length(p.xy), p.z)) - 1;
  float result = min(max(d.x, d.y), 0) + length(max(d, 0));
  result = max(result, -p.y);
  return result;
}

float poll2(vec3 p) {
  p = translate(p, vec3(-4.2,1.5,0));
  p = rotate(p, vec3(0,0,1), 20);
  p = scale(p, vec3(2, 1.5, 15));
  vec2 d = abs(vec2(length(p.xy), p.z)) - 1;
  float result = min(max(d.x, d.y), 0) + length(max(d, 0));
  return result;
}

float body(vec3 p) {
  float sub3 = p.y + 0.5;
  const float freq = 0.1;
  float triWave = (1 - abs(mod(p.x*10,2)-1)) * 0.2;
  vec3 n = normalize(vec3(-0.3, 1, 0));
  float sub2 = dot(p - vec3(-4.2, 1.2, 0) - n * triWave, normalize(-n));
  p = translate(p, vec3(10 / 18. + 0.3, 0,0));
  float result = length(max(abs(p) - vec3(10,1,1), 0.0f));
  float sub = dot(p - vec3(0,0.5,0), normalize(vec3(0.15,1,0)));
  result = max(result, sub);
  result = max(result, -sub2);
  result = max(result, -sub3);
  return result;
}

float ringOnly(vec3 p) {
  float rate = 1 + (sin(uni.time /10) + 1) * 0.05;
  p = scale(p, vec3(rate,1,rate));
  vec2 q = vec2(length(p.xy) - 4, p.z);
  float result = length(q) - 0.3;
  return result;
}

float ball(vec3 p) {
  p = scale(p, vec3(0.5,0.5,10) );
//  p = translate(p, vec3(0,-7,0));
  return length(p) - 1;
}

float side(vec3 p) {
  float sub3 = length(p - vec3(0,-4,0)) - 1.2;
  p = scale(p, vec3(0.65, 1,1));
  p.x = abs(p.x);
  p = translate(p, vec3(1.2,0,0));
  p = rotate(p, vec3(0,0,1), 74 + 8 * (sin(uni.time / 10) + 1));
  //p = rotate(p, vec3(0,0,1), 76);
  p = translate(p, vec3(0.5, -1.4, 0));
  float sub2 = poll2(p);
  float result = body(p);
  result = smin(result, poll(p), 4);
  result = max(result, abs(p.z) - 1);
  result -= 0.5;
  result = max(result, -sub2);
  result = max(result, -sub3);
  return result;
}

float ring(vec3 p) {
  float sub3 = length(p - vec3(0,-4,0)) - 1.2;
  float r = ringOnly(p);
  p = scale(p, vec3(0.65, 1,1));
  p.x = abs(p.x);
  p = translate(p, vec3(1.2,0,0));
  p = rotate(p, vec3(0,0,1), 74 + 8 * (sin(uni.time / 10) + 1));
  //p = rotate(p, vec3(0,0,1), 76);
  p = translate(p, vec3(0.5, -1.4, 0));
  float sub2 = poll2(p);
  float result = r;
  result = max(result, -sub2);
  result = max(result, -sub3);
  return result;
}

float dist(vec3 p) {
//  float py = p.y+8;
//  p.x+=(1-1/pow(py*py+1,0.4))*pow(py,1.8)*0.05*sin(uni.time/2);
//  p.z+=(1-1/pow(py*py+1,0.4))*pow(py,1.8)*0.05*sin(uni.time/1.34235324);
//  float result = side(p);
//  result = min(result, ring(p));
//  return result-0.5-0.1*sin(uni.time*0.2)*pow((sin(p.x*4)+sin(p.y*4)+sin(p.z*4)),2);
//  return result;
  float result = side(p);
  result = min(result, ring(p));
//  return result-0.2-0.08*(sin(uni.time/3+p.x*p.x*0.4)+sin(20 * uni.time/3+p.y*p.y*0.10)+sin(uni.time/3+p.z*p.z*0.3));
  return result;
}

int getNearestIndex(vec3 p) {
  float minDist = side(p);
  int idx = 0;
  float d = ring(p);
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


void main() {
  vec3 eye = vec3(0,0,-20);
  eye = (uni.rot * vec4(eye,0)).xyz;
  vec3 ray = vec3(tc, sqrt(2) * tan(fov));
  const float angle = 1.5;
  ray = vec3(sin(tc * angle), length(cos(tc * angle)));
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
    vec4 reflectColor = texture(mTexture, cubeMap(current, reflect(-eyeVec, n)));
    switch (getNearestIndex(current)) {
      case 0:
        float refRate = 0.05;
        FragColor = vec4(0.9,0.7,0.7,1) * diffuse;
        FragColor = FragColor * (1-refRate) + reflectColor * refRate;
        FragColor += pow(spec, 5) * 0.2;
        break;
      case 1:
        FragColor = reflectColor * 0.2 + vec4(0.5, 0.5 ,0.5, 1) * 0.8;
        FragColor *= diffuse;
        FragColor += pow(spec, 5);
        break;
      case 2:
        FragColor = vec4(0,0,0,1) * diffuse;
        break;
      default:
        FragColor = vec4(0,0,1,1);
        break;
    }
  } else {
    FragColor = vec4(0.25);
    FragColor.a = 1;
  }
}
};

struct Uniform {
align(16):
    float time;
    mat4 rot;
}

alias Basami = FragmentCanvas!(fragmentSource, Uniform, 1);
