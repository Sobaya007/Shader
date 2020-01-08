import sbylib;

void handleContext(E)(ModuleContext context, E entity) {
    when(context.unbound).then({
        entity.unregister();
    });
    when(context.bound).then({
        entity.reregister();
    });
    context.unbind();
}
