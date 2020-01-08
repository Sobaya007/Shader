import std : exp, writeln;
import sbylib;
import frag;
import rot;
import util;

mixin(Register!entryPoint);

@depends("root")
void entryPoint(ModuleContext context, Window window, ModuleContext[string] contexts) {
    auto bolt = Bolt.create(window);
    context.pushResource(bolt);

    Rot q;
    float t = 0;
    vec3 c = vec3(0.9,0.8,0.4);
    
    with (context()) {
        when(Frame).then({
            float s = t;
            s = 1 - exp(-s * 0.01);
            with (bolt.fragmentUniform) {
                rot = q.toMatrix4;
                lightColor = c * s;
            }
            t++;
        });

        when(KeyButton.KeyT.pressed.on(window)).then({
            t = 0;
        });
        handleContext(context, bolt);
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
    mat4 rot;
    vec3 lightColor;
} uni;

float po(vec3 start, vec3 ray, vec3 point) {
  ray = normalize(ray);
  return length(dot(point-start, ray) * ray + start - point);
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

float head(vec3 p) {
  p = translate(p, vec3(0,3.6,0));
  if (p.y < 0) p = scale(p, vec3(1,1.2,1));
  else p = scale(p, vec3(1,1,1));
  return length(p) - 3.4;
}

float spring(vec3 p) {
  const float r = 0.3;
  const float R = 2.5;
  float sub = p.y - 1;
  float sub2 = -(p.y + 4);
  float angle = atan(p.x, p.z);
  if (angle < 0) angle += 2 * pi;
  if (angle > 2 *pi) angle -= 2 * pi;
  angle = -angle;
  float a = 0.5 - angle / (2*pi) * 1;
  p.y = mod(p.y + a, 4 * r)- a;
  angle *= 1 / (2 * pi) * 4 * r;
  p = translate(p, vec3(0,angle,0));
  vec2 q = vec2(length(p.xz) - R, p.y);
  float result = length(q) - r;
  result = max(result, sub);
  result = max(result, sub2);
  return result;
}

float socket(vec3 p) {
  float spr = spring(p - vec3(0,-1.7,0));
  {
    float rate = 1;
    const float start = 1.3;
    const float interval = 0.2;
    const float startRate = 1.1;
    if (p.y > start) {
      if (p.y < start + 0.2) rate = startRate;
    }
    else if (p.y > start - interval) rate = 1 + (startRate - 1) * (p.y - start + interval) / interval;
    p = scale(p, vec3(rate,1,rate));
  }
  p = translate(p, vec3(0,-1,0));
  {
    float rate = 1;
    const float interval = 0.3;
    const float start = -0.4;
    const float arrivalRate = 0.92;
    if (p.y < start - interval) rate = arrivalRate;
    else if (p.y < start) rate = 1 + (p.y - start) / interval * (1 - arrivalRate);
    p = scale(p, vec3(rate,1.7,rate));
  }
  vec2 d = abs(vec2(length(p.xz),p.y)) - 2.8;
  float result = min(max(d.x,d.y),0.0) + length(max(d,0.0));
  result = smin(result, spr, 10);
  return result;
}

float ketsu(vec3 p) {
  const float size = 6;
  const float rate = 8. / 11;
  const float r = 2.4;
  const float h = 0.1;
  float sphere = length(p - vec3(0,sqrt(r*r-h*h) - 7.5,0)) - r;
  p = translate(p, vec3(0, -7,0));
  p = translate(p, vec3(0, -size*rate,0));
  p = scale(p, vec3(0.5,1,0.5));
  vec2 q = vec2(length(p.xz), p.y);
  float result = dot(q, normalize(vec2(1.5,-1)));
  result = max(result, p.y - size);
  result = max(result, -(p.y - size *rate));
  result = min(result, max(sphere, p.y - size * rate));
  return result;
}

float wall(vec3 p) {
  float result = p.z + 10;
  result = min(result, p.y + 10);
  return result;
}

float dist(vec3 p) {
  float result = head(p);
  result = min(result, socket(p));
  result = min(result, ketsu(p));
  result = min(result, wall(p));
  return result;
}

int getNearestIndex(vec3 p) {
  float minDist = head(p);
  int idx = 0;
  float d = socket(p);
  if (d < minDist) {
    minDist = d;
    idx = 1;
  }
  d = ketsu(p);
  if (d < minDist) {
    minDist = d;
    idx = 2;
  }
  d = wall(p);
  if (d < minDist) {
    minDist = d;
    idx = 3;
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

vec3 skyMap(vec3 vec) {
  float t = atan(vec.z, vec.x);
  float p = asin(vec.y);
  p = (p * 2 / pi + 1) / 2;
  return vec3(1);
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

float cookTorrance(vec3 N, vec3 lightPos, vec3 eye, vec3 current, float m, float n) {
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

vec3 getHeadColor(vec3 eye, vec3 ray) {
  const float thickness = 0.05;
  const float r = 3.4;
  vec3 v = center - eye;
  vec3 n = getNormal(eye);
  vec3 current = rayMarch(eye, ray);
  float d = dot(v, ray);
  float len = length(v);
  float l = 2 * r * thickness / sqrt(d*d+r*r-len*len); //pass through surface
  vec3 result = 0.1 * skyMap(ray) * exp(-l);
  float spec = cookTorrance(n, lightPos, eye, current, 0.1, 1.5);
  float s = 1;
  float toNext = 2 * dot(ray, center - current);
  float light = 0;
  for (int i = 0; i < 10; i++) {
    vec3 before = current;
    const float delta = 0.01;
    for (int j = 0; j < toNext / delta; j++) {
      current += delta * ray;
      light += 0.2 * pow(length(current - center), -2) * delta * s; 
    } 
    n = -getNormal(current);
    spec += cookTorrance(n, lightPos, before, current, 0.1, 1.5) * s;
    ray = reflect(ray, n);
    current += ray * 0.2;
    v = current - center;
    d = dot(v, ray);
    len = length(v);
    l = 2 * r * thickness / sqrt(d*d+r*r-len*len);
    s *= 0.5;
  }
  result += uni.lightColor * (light + 0.1);
  result += ambientColor;

  return result;
}

vec3 getWallColor(vec3, vec3);

vec3 getMetalColor(vec3 eye, vec3 ray) {
  vec3 originalRay = ray;
  vec3 current = rayMarch(eye, ray);
  vec3 n = getNormal(current);
  vec3 result;

  vec3 reflectColor = skyMap(reflect(normalize(current - eye), n));
  result = reflectColor;
  const float gamma = 3;
  result.r = pow(result.r, gamma);
  result.g = pow(result.g, gamma);
  result.b = pow(result.b, gamma);

  float spec = cookTorrance(n, lightPos*0.5+eye*0.5, eye, current, 0.5, 20);
  result *= spec;

  ray = reflect(ray, n);
  current += ray * 3;
  int idx = getNearestIndex(rayMarch(current, ray));

  switch (idx) {
    case 0:
      result += getHeadColor(current, ray) * 0.5;
      break;
    case 3:
      result += getWallColor(current, ray) * 0.5;
      break;
  }

  result += ambientColor;
  result += uni.lightColor / pow(po(eye,originalRay,lightPos),2.0) * 2.0;

  return result;
}

vec3 getWallColor(vec3 eye, vec3 ray) {
  vec3 current = rayMarch(eye, ray);
  vec3 result = 20 * uni.lightColor * pow(length(current - center)-3.4, -2);
  result += ambientColor;
  return result;
}

void main() {
  vec2 uv = tc;
  vec3 eye = vec3(0,0,40);
  eye = (uni.rot * vec4(eye,0)).xyz;
  vec3 ray = vec3(uv * 2 * tan(fov/2), -1);
  ray = (uni.rot * vec4(ray,0)).xyz;
  ray = normalize(ray);
  vec3 current = rayMarch(eye, ray);

  if (abs(dist(current)) < eps) {
    vec3 n = getNormal(current);
    vec3 eyeVec = normalize(eye - current);
    float diffuse = max(0,dot(eyeVec, n));
    float light = 0;
    switch (getNearestIndex(current)) {
      case 0:
        FragColor.rgb = getHeadColor(eye, ray);
        break;
      case 1:
        FragColor.rgb = getMetalColor(eye, ray);
        break;
      case 2:
        FragColor.rgb = ambientColor;
        break;
      case 3:
        FragColor.rgb = getWallColor(eye, ray);
        break;
      default:
        FragColor.rgb = vec3(0,0,1);
        break;
    }
  } else {
    FragColor.rgb = vec3(0);
  }
  FragColor.a = 1;
}
};

struct Uniform {
align(16):
    mat4 rot;
    vec3 lightColor;
}

alias Bolt = FragmentCanvas!(fragmentSource, Uniform);
