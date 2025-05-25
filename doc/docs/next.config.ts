import nextra from 'nextra'

import { remarkCodeHike, recmaCodeHike, CodeHikeConfig } from "codehike/mdx"
import { PluggableList } from "unified";

const chConfig: CodeHikeConfig = {
    // optional (see code docs):
    components: { code: "Code" },
    // if you can't use RSC:
    syntaxHighlighting: {
        theme: "github-dark",
    },
};

const mdxOptions: { remarkPlugins: PluggableList, recmaPlugins: PluggableList } = {
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
    webpack(config) {
        // Grab the existing rule that handles SVG imports
        const fileLoaderRule = config.module.rules.find((rule: any) => rule.test?.test?.('.svg'))

        config.module.rules.push(
            // Reapply the existing rule, but only for svg imports ending in ?url
            {
                ...fileLoaderRule,
                test: /\.svg$/i,
                resourceQuery: /url/, // *.svg?url
            },
            // Convert all other *.svg imports to React components
            {
                test: /\.svg$/i,
                issuer: fileLoaderRule.issuer,
                resourceQuery: { not: [...fileLoaderRule.resourceQuery.not, /url/] }, // exclude if *.svg?url
                use: ['@svgr/webpack'],
            },
        )

        // Modify the file loader rule to ignore *.svg, since we have it handled now.
        fileLoaderRule.exclude = /\.svg$/i

        return config
    },
})
