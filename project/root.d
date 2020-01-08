import std;
import sbylib;

mixin(Register!entryPoint);
void entryPoint(Project proj, ModuleContext context, Window window) {
    proj["contexts"] = (ModuleContext[string]).init;

    with (context()) {
        when(KeyButton.KeyR.pressed.on(window)).then({
            proj.reloadAll();
        });

        when(KeyButton.Enter.pressed.on(window)).then({
            next(proj, window);
        });
        when(KeyButton.Space.pressed.on(window)).then({
            RenderContext(window).screenShot("screenshot.png");
        });
        next(proj, window);
    }
}

Nullable!size_t current;
void next(Project proj, Window window) {
    if (!current.isNull) {
        proj.moduleList.values[current.get()].context.unbind();
        do {
            current = (current.get() + 1) % proj.moduleList.values.length;
        } while (proj.moduleList.values[current.get()].name == "root");
    } else {
        current = 0;
    }
    window.title = proj.moduleList.values[current.get()].name;
    proj.moduleList.values[current.get()].context.bind();
}
