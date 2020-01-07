import std : Nullable, nullable;
import sbylib;

struct Rot {
    quat q = quat(0,0,0,1);
    private quat aq = quat(0,0,0,1);
    private Nullable!vec2 m;

    alias q this;

    void registerEvent(Window window) {
        when(Frame).then({
            q += (aq - q) * 0.01;
        });
        when(MouseButton.Button1.pressed.on(window)).then({
            m = mouse.pos.on(window).nullable;
        });
        when(mouse.moved.on(window)).then({
            if (m.isNull) return;

            auto n = mouse.pos.on(window);
            scope (exit) m = n.nullable;

            auto d = n - m.get();
            auto angle = d.length.rad * 0.003;
            auto axis = safeNormalize(aq.toMatrix3 * vec3(d.yx, 0));
            aq = quat.axisAngle(axis, angle) * aq;
            auto forward = aq.baseZ;
            auto side = normalize(cross(vec3(0,1,0), forward));
            auto up = normalize(cross(forward, side));
            aq = mat3(side, up, forward).toQuaternion;
        });
        when(MouseButton.Button1.released.on(window)).then({
            m.nullify();
        });
    }
}
