import defaultMdxComponents from 'fumadocs-ui/mdx';
import type { MDXComponents } from 'mdx/types';
import { Code } from './lib/components/code';
import { ScrollyCoding } from "@/lib/components/scrollycoding";
import { Callout } from "fumadocs-ui/components/callout";
import { ImageZoom } from "fumadocs-ui/components/image-zoom";
import { Mermaid } from '@/lib/components/Mermaid';

// use this function to get MDX components, you will need it for rendering MDX
export function getMDXComponents(components?: MDXComponents): MDXComponents {
  return {
    ...defaultMdxComponents,
    Code: Code,
    ScrollyCoding: ScrollyCoding,
    Mermaid,
    mermaid: Mermaid,
    DocTooltip: ({ children }: { children: React.ReactNode }) => (
      <div className="doctooltip">{children}</div>
    ),
    TooltipTarget: () => <></>,
    UnderConstruction: () => <Callout type={"warning"}>Under construction.</Callout>,
    img: (props) => <ImageZoom {...(props as any)} />,
    ...components,
  };
}
