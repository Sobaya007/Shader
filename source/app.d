import std;
import sbylib;
import erupted;

void main() {
    Window window;
    with (WindowBuilder()) {
        width = 400;
        height = 400;
        title = "Shader Example";
        clientAPI = ClientAPI.NoAPI;
        floating = true;
        resizable = false;
        window = buildWindow();
    }
    enforce(GLFW.hasVulkanSupport());
    VulkanContext.initialize("example app", VK_MAKE_VERSION(0,0,1), window);
    scope (exit) VulkanContext.deinitialize();

    when(KeyButton.Escape.pressed.on(window)).then({
        window.shouldClose = true;
    });
    when(window.shouldClose).then({
        stopEngine();
    });
    when(Frame(88)).then({
        StandardRenderPass(window).submitRender();
    });
    when(Frame(90)).then({
        GLFW.pollEvents();
    });

    EngineSetting setting = {
        projectDirectory: "project"
    };
    startEngine(setting, [
        "window": Variant(window)
    ]);
}
