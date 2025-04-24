import nextra from 'nextra'

import {remarkCodeHike, recmaCodeHike, CodeHikeConfig} from "codehike/mdx"
import {PluggableList} from "unified";

const chConfig: CodeHikeConfig = {
    // optional (see code docs):
    components: {code: "Code"},
    // if you can't use RSC:
    syntaxHighlighting: {
      theme: "github-dark",
    },
};

const mdxOptions: {remarkPlugins: PluggableList, recmaPlugins: PluggableList} = {
    remarkPlugins: [[remarkCodeHike, chConfig]],
    recmaPlugins: [[recmaCodeHike, chConfig]],
}

const withNextra = nextra({
    // ... Other Nextra config options
    mdxOptions: {
        remarkPlugins: [[remarkCodeHike, chConfig]],
        recmaPlugins: [[recmaCodeHike, chConfig]],
    },
    codeHighlight: false,
    latex: { renderer: 'mathjax' },
})

// You can include other Next.js configuration options here, in addition to Nextra settings:
export default withNextra({
    // ... Other Next.js config options
    output: 'export',
})
