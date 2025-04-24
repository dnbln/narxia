import { useMDXComponents as getThemeComponents } from 'nextra-theme-docs' // nextra-theme-blog or your custom theme
import {MDXComponents} from "mdx/types";
import {Code} from "@/components/code";
import {ScrollyCoding} from "@/components/scrollycoding";
import {Callout} from "nextra/components/callout";

// Get the default MDX components
const themeComponents = getThemeComponents({
    Code: Code,
    ScrollyCoding: ScrollyCoding,
    UnderConstruction: () => <Callout type={"warning"}>Under construction.</Callout>,
})

// Merge components
export function useMDXComponents(components: MDXComponents) {
    return {
        ...themeComponents,
        ...components,
    }
}
