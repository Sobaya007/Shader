import std;
import sbylib;

mixin(Register!entryPoint);
void entryPoint(Project proj, ModuleContext context, Window window) {
    proj["contexts"] = (ModuleContext[string]).init;

    with (context()) {
        when(KeyButton.KeyR.pressed.on(window)).then({
            proj.reloadAll();
        });

        Nullable!size_t current;
        when(KeyButton.Enter.pressed.on(window)).then({
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
        });
    }
}
