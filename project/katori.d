import std : exp, writeln;
import sbylib;
import frag;
import rot;

// mixin(Register!entryPoint);
void entryPoint(Project proj, ModuleContext context, Window window) {
    auto tex = new FileTexture("resource/spheremap.jpg");
    context.pushResource(tex);
    auto bolt = Katori.create(window, tex);
    context.pushResource(bolt);

    Rot q;
    float t = 0;
    
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

float length8(vec2 p) {
  float x = 1, y = 1;
  for (int i = 0; i < 8; i++) {
    x *= p.x;
    y *= p.y;
  }
  return pow(x + y, 1.0 / 8);
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

vec3 fireEnd(float t) {
  float n = floor((20 - t * 0.01) / (2 * pi));
  float angle = 20 - t * 0.01 - n * 2 * pi;
  float len = angle * 0.48 + 3 * n + 1;
  return vec3(sin(angle) * len, 0, cos(angle) * len);
}

float fire(vec3 p) {
  p = translate(p, fireEnd(uni.time) + vec3(0,5,0));
  p = scale(p, vec3(2,8,2)*4);
  vec2 d = abs(vec2(length(p.xz),p.y)) - 1;
  return min(max(d.x,d.y),0.0) + length(max(d,0.0));
}

float body(vec3 p) {
  p = translate(p, vec3(0.4,3,0));
  float sub = p.y + 0.0;
  float len = length(p.xz);
  float angle = atan(p.x, p.z);
  if (angle < 0) angle += 2 * pi;
  if (angle > 2 * pi) angle -= 2 * pi;
  float n = floor((len - angle * 0.48+1) / 3 - 0.2);
  float t = angle + n * 2 * pi;
  if (t > 20 - uni.time * 0.01) return 1;
  vec2 q = vec2(mod(len - angle * 0.48, 3)-1, p.y);
  float result = length8(q) - 0.5;
  result = max(result, sub);
  //result -= noise(p * 300) * 0.01;
  return result;
}

float distSaucer(vec3 p) {
  p = translate(p,vec3(0,-3,0));
  vec2 q = abs(vec2(length(p.zx), p.y / 20)) - 0.3;
  float bar = min(max(q.x,q.y),0) + length(max(q,0));
  q = vec2(length(p.xz) - 10, p.y + 1.0);
  float torus = length(q) - 0.2;
  float bottom2 = max(max(-p.y-3.9, p.y + 3.5), length(p-vec3(0,-2,0)) - 10);
  p = translate(p, vec3(0,-1,0));
  p = scale(p, vec3(1,30,1));
  q = vec2(length(p.xz) - 10, p.y);
  float result = length8(q) - 0.1;
  result = min(result, bottom2);
  result = min(result, torus);
  result = min(result, bar);
  return result;
}

float dist(vec3 p) {
//  return body(p);
  return min(body(p), distSaucer(p));
}

int getNearestIndex(vec3 p) {
  float minDist = body(p);
  int idx = 0;
  float d = distSaucer(p);
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
  for (int i = 0; i < 500; i++) {
    float d = dist(current);
    current += ray * d;
    if (abs(d) < eps) {
      break;
    }
  }
  return current;
}

vec3 rayMarchFire(vec3 eye, vec3 ray) {
  vec3 current = eye;
  for (int i = 0; i < 500; i++) {
    float d = fire(current);
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

vec3 getKatoriColor(vec3 p, vec3 vec) {
  vec3 n = getNormal(p);
  float diffuse = max(0,dot(-vec, n));
  vec3 difColor = vec3(0,0.5,0.3) * 0.8;
  float len = length((p - fireEnd(uni.time)).xz);
  vec3 gray = vec3(0.7);
  vec3 red = vec3(0.4,0,0);
  vec3 black = vec3(0);
  if (len < 0.7) {
    difColor = gray;
  } else if (len < 0.85) {
    float t = (len - 0.7) / 0.15;
    difColor = gray * (1-t) + red * t;
  } else if (len < 0.95) {
    difColor = red;
  } else if (len < 1.1) {
    float t = (len - 0.95) / 0.15;
    difColor = red * (1-t) + black * t;
  } else if (len < 1.2) {
    difColor = black;
  }
  return difColor * diffuse;
}

float getShadow(vec3 p, vec3 vec) {
  float minDist = 114514;
  float all = 0;
  for (int i = 0; i < 100; i++) {
    float d = body(p);
    if (abs(d) < eps) {
      return 0.5;
    }
    minDist = min(minDist, abs(d) * 16 / all);
    p += vec * d;
    all += d;
  }
  return 0.5 + 0.5 * minDist;
}

vec3 getSaucerColor(vec3 p, vec3 vec) {
  vec3 lightPos = vec3(4,30,2);
  float s = getShadow(lightPos, normalize(p - lightPos));
  float diffuse = max(0,dot(-vec, getNormal(p)));
  vec = reflect(vec, getNormal(p));
  p += vec;
  int idx = 1;
  for (int j = 0; j < 3; j++) {
    p = rayMarch(p, vec);
    if (abs(dist(p)) > eps) break;
    if (getNearestIndex(p) == 0) {
      idx = 0;
      break;
    }
    vec = reflect(vec, getNormal(p));
  }
  vec3 reflectColor;
  switch (idx) {
    case 0:
      reflectColor = getKatoriColor(p, vec);
      break;
    case 1:
      reflectColor = texture(mTexture, cubeMap(p, reflect(vec, getNormal(p)))).rgb;
      break;
  }

  float refRate = 0.2;
  vec3 result = reflectColor * refRate + vec3(1) * (1-refRate) * diffuse;
  result *= 1 - s;
  return result;
}

void main() {

  vec3 eye = vec3(0,0,-15);
  eye = (uni.rot * vec4(eye,0)).xyz;
  vec3 ray = vec3(tc * 2 * tan(fov/2), 1);
  ray = (uni.rot * vec4(ray,0)).xyz;
  ray = normalize(ray);
  vec3 current = rayMarch(eye, ray);

  vec3 lightPos = vec3(1,1,-1) * 10;

  if (abs(dist(current)) < eps) {
    switch (getNearestIndex(current)) {
      case 0:
        FragColor.rgb = getKatoriColor(current, normalize(current - eye));
        break;
      case 1:
        FragColor.rgb = getSaucerColor(current, normalize(current - eye));
        break;
      default:
        FragColor.rgb = vec3(1);
        break;
    }
  } else {
    FragColor.rgb = vec3(0);
  }
  FragColor.a = 1;
 
  current = rayMarchFire(eye, ray);
  float t = 0;
  vec3 firePos = fireEnd(uni.time+5);
  for (int i = 0; i < 20; i++) {
    if (current.y < 2.5) continue;
    float noiseValue = noise((current-firePos) * 300 - uni.time * 20);
    noiseValue *= 1 - length((current-firePos).xz) * 3 ;
    noiseValue = clamp(noiseValue, 0, 1);
    noiseValue = pow(noiseValue, 2);
    t += noiseValue * 0.1;
    current += ray * 0.5;
  }
  FragColor.rgb += t * vec3(0.8) * 10;
}
};

struct Uniform {
align(16):
    float time;
    mat4 rot;
}

alias Katori = FragmentCanvas!(fragmentSource, Uniform, 1);
