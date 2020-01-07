import std : uniform, sqrt, writeln;
import sbylib;
import frag;
import rot;

// mixin(Register!entryPoint);
void entryPoint(Project proj, ModuleContext context, Window window) {
    auto tex = new FileTexture("resource/dwango.png");
    context.pushResource(tex);

    auto dwango = Dwango.create(window, tex);
    context.pushResource(dwango);

    Rot q;
    float t = 0;

    auto height = 0.05;
    auto width = height * tex.width / tex.height;
    vec4[10] cps;
    foreach (ref c; cps) {
        c.xy = vec2(uniform(-width-1, 1.0f), uniform(0, 1.0f - height));
    }
    
    with (context()) {
        when(Frame).then({
            scope (exit) t++;
            quat sunQ = quat.axisAngle(vec3(0,1,1).normalize, t * 0.003.rad);
            vec3 sun = vec3(0,1,-1) * 30;
            sun = sunQ.rotate(sun);
            if (sun.y < 0) sun = -sun;
            foreach (ref c; cps) {
              c.x -= 0.0005;
              if (c.x + width < -1) {
                c.x = 1;
                c.y = uniform(0, 1 - height-0.2);
              }
            }
            with (dwango.fragmentUniform) {
                rot = q.toMatrix4;
                lightPos = sun;
                // c.setTexture(tex);
                foreach (i, ref cp; cps) {
                    cPos[i] = cp;
                }
                cSize = vec2(width, height);
            }
        });

        when(KeyButton.KeyR.pressed.on(window)).then({
            proj.reloadAll();
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
    vec3 lightPos;
    mat4 rot;
    vec4 cPos[10];
    vec2 cSize;
} uni;
layout(binding=1) uniform sampler2D mTexture;

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

float distPlane(vec3 p) {
  return p.y + 2.8;
}

float distEye(vec3 p) {
  p = rotate(p, vec3(0,0,1), 10);
  p.x = abs(p.x);
  p = translate(p, vec3(1,0.4,+1.8));
  p = scale(p, vec3(1,1,0.5));
  return length(p) - 0.25;
}

float distMouse(vec3 p) {
  p = translate(p, vec3(0,-0.7,+0.5));
  vec3 q = abs(p);
  return max(q.z-1.5,max(q.x*0.866025+p.y*0.5,-p.y)-0.4*0.5);
}

float distFoot(vec3 p) {
  p.x = abs(p.x);
  p = translate(p, vec3(1.5,-1.5,+0.5));
  p = scale(p,vec3(0.5,1.2,0.5));
  return length(p) - 1;
}

float distAntenna(vec3 p) {
  p.x = abs(p.x);
  p = translate(p, vec3(0,2,0));
  p = rotate(p, vec3(0,0,1), -45);
  vec3 size = vec3(0.1,2,0.1);
  return length(max(abs(p) - size, 0));
}

float distBody(vec3 p) {
  vec3 size = vec3(1.4,0.9,0.8);
  return length(max(abs(p) - size, 0)) - 1;
}

float dist(vec3 p) {
  float result = min(
      distBody(p),
      min(
      distAntenna(p),
      min(
      distFoot(p),
      min(
      distEye(p),
      min(
      distMouse(p),
      distPlane(p)
      )))));
  return result;
}

int getNearestIndex(vec3 p) {
  float dist = distBody(p);
  int idx = 0;
  float d = distAntenna(p);
  if (d < dist) {
    dist = d;
    idx = 1;
  }
  d = distFoot(p);
  if (d < dist) {
    dist = d;
    idx = 2;
  }
  d = distEye(p);
  if (d < dist) {
    dist = d;
    idx = 3;
  }
  d = distMouse(p);
  if (d < dist) {
    dist = d;
    idx = 4;
  }
  d = distPlane(p);
  if (d < dist) {
    dist = d;
    idx = 5;
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

void drawBackground(vec3 eye, vec3 ray) {
  const vec2 screenSize = vec2(10,8) * 20;
  const float z = -50;
  float s = (z - eye.z) / ray.z;
  vec2 texCoord = (eye+ray*s).xy;
  texCoord += screenSize / 2;
  texCoord = mod(texCoord, screenSize);
  texCoord /= screenSize;
  for (int i = 0; i < 10; i++) {
    vec2 cp = uni.cPos[i].xy;
    vec2 end = cp + uni.cSize;
    if (cp.x < texCoord.x && texCoord.x < end.x
        && cp.y < texCoord.y && texCoord.y < end.y) {
      vec2 t = texCoord;
      t -= cp;
      t /= uni.cSize;
      vec4 col = texture(mTexture, t);
      FragColor.rgb += col.rgb * (col.a > 0 ? 1 : 0);
    }
  }

}

void main() {
  vec3 eye = vec3(0,0,+5);
  eye = (uni.rot * vec4(eye,0)).xyz;
  vec3 ray = vec3(tc * 2 * tan(fov/2), -1);
  ray = (uni.rot * vec4(ray,0)).xyz;
  ray = normalize(ray);
  vec3 current = rayMarch(eye, ray);

  if (abs(dist(current)) < eps) {
    switch (getNearestIndex(current)) {
      case 1:
      case 3:
      case 4:
        FragColor = vec4(0,0,0,1);
        break;
      case 0:
        FragColor = vec4(0.9);
        vec3 lightVec = normalize(uni.lightPos - current);
        vec3 viewVec = normalize(eye - current);
        vec3 normal = getNormal(current);
        vec3 halfVec = normalize(lightVec + viewVec);
        float diffuse = dot(lightVec, normal);
        diffuse = max(0,diffuse);
        float spec = dot(normal, halfVec);
        spec = max(0,spec);
        FragColor.rgb *= diffuse; 
        FragColor.rgb += pow(spec, 2.5);
        current.xy = abs(current.xy);
        if (current.z > -1.7) break;
        if (1.7 < current.x && current.x < 1.8)
          FragColor = vec4(0,0,0,1);
        if (current.x < 1.8 && 1.2 < current.y && current.y < 1.5)
          FragColor = vec4(0,0,0,1);
        break;
      case 5:
        FragColor = vec4(0);
        drawBackground(eye, ray);
        FragColor /= 2;
        FragColor += vec4(0.43, 0.1, 0.08, 1);
        ray = normalize(current - uni.lightPos);
        current = rayMarch(uni.lightPos, ray);
        if (getNearestIndex(current) != 5) {
          FragColor *= 0.5;
        }
        break;
      default:
        FragColor = vec4(0.9);
        break;
    }
  } else {
    FragColor = vec4(0.25);
    FragColor.a = 1;
    drawBackground(eye, ray);
  }
}
};

struct Uniform {
align(16):
    vec3 lightPos;
    mat4 rot;
    vec4[10] cPos;
    vec2 cSize;
}

alias Dwango = FragmentCanvas!(fragmentSource, Uniform, 1);
