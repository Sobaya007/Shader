import std;
import erupted;
import sbylib;
import sbylib.graphics : DefineMaterial = Material;

class FragmentMaterial(alias fragSource, FragmentUniform) {

    enum MaxObjects = 10;

    mixin DefineMaterial!(DataSet);

    struct Vertex {
        vec3 position;
    }

    private @own ShaderModule[] shaders;
    private @own Pipeline pipeline;

    mixin ImplReleaseOwn;

    this(Window window, PrimitiveTopology topology) {
        initialize(MaxObjects);
        with (RenderContext) {
            Pipeline.GraphicsCreateInfo pipelineCreateInfo = {
                stages: [createStage(ShaderStage.Vertex, shaders, q{
                    #version 450
                    layout (location = 0) in vec3 position;
                    layout (location = 0) out vec2 tc;

                    void main() {
                        gl_Position.x = +position.x * 2;
                        gl_Position.y = -position.y * 2;
                        tc = position.xy;
                    }
                }), createStage(ShaderStage.Fragment, shaders, fragSource)],
                vertexInputState: getVertexInputState!(Vertex),
                inputAssemblyState: {
                    topology: topology
                },
                viewportState: {
                    viewports: [{
                        x: 0.0f,
                        y: 0.0f,
                        width: window.width,
                        height: window.height,
                        minDepth: 0.0f,
                        maxDepth: 1.0f
                    }],
                    scissors: [{
                        offset: {
                            x: 0,
                            y: 0
                        },
                        extent: {
                            width: window.width,
                            height: window.height
                        }
                    }]
                },
                rasterizationState: {
                    depthClampEnable: false,
                    rasterizerDiscardEnable: false,
                    polygonMode: PolygonMode.Fill,
                    cullMode: CullMode.None,
                    frontFace: FrontFace.CounterClockwise,
                    depthBiasEnable: false,
                    depthBiasConstantFactor: 0.0f,
                    depthBiasClamp: 0.0f,
                    depthBiasSlopeFactor: 0.0f,
                    lineWidth: 1.0f,
                },
                multisampleState: {
                    rasterizationSamples: SampleCount.Count1,
                    sampleShadingEnable: false,
                    alphaToCoverageEnable: false,
                    alphaToOneEnable: false,
                },
                depthStencilState: {
                    depthTestEnable: true,
                    depthWriteEnable: true,
                    depthCompareOp: CompareOp.Less,
                },
                colorBlendState: {
                    logicOpEnable: false,
                    attachments: [{
                        blendEnable: false,
                        colorWriteMask: ColorComponent.R
                                      | ColorComponent.G
                                      | ColorComponent.B
                                      | ColorComponent.A,
                    }]
                },
                layout: pipelineLayout,
                renderPass: StandardRenderPass(window),
                subpass: 0,
            };
            this.pipeline = Pipeline.create(VulkanContext.device, [pipelineCreateInfo])[0];
        }
    }

    static class DataSet {
        mixin StandardDataSet;
        mixin UseVertex!(Vertex);
        mixin UseIndex!(uint);
        @binding(0) mixin UseFragmentUniform!(FragmentUniform);
        // @type(DescriptorType.CombinedImageSampler) @stage(ShaderStage.Fragment) @binding(1) Texture texture;
        mixin ImplDescriptorSet;
        mixin ImplReleaseOwn;
        mixin ImplRecord;

        this(Geometry)(Geometry geom, DescriptorPool descriptorPool, DescriptorSetLayout descriptorSetLayout) {
            initializeVertexBuffer(geom);
            initializeIndexBuffer(geom);
            initializeFragmentUniform();
            initializeDescriptorSet(descriptorPool, descriptorSetLayout);
        } }
}

class FragmentCanvas(alias fragSource, FragmentUniform) {
    mixin ImplPos;
    mixin ImplWorldMatrix;
    mixin UseMaterial!(FragmentMaterial!(fragSource, FragmentUniform));

    static create(Window window) {
        return new typeof(this)(window, GeometryLibrary().buildPlane());
    }
}
