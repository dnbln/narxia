import {
  defineConfig,
  defineDocs,
  frontmatterSchema,
  metaSchema,
} from 'fumadocs-mdx/config';
import { remarkCodeHike, recmaCodeHike, CodeHikeConfig } from "codehike/mdx"
import rehypeKatex from 'rehype-katex'
import remarkMath from 'remark-math'

// You can customise Zod schemas for frontmatter and `meta.json` here
// see https://fumadocs.vercel.app/docs/mdx/collections#define-docs
export const docs = defineDocs({
  docs: {
    schema: frontmatterSchema,
  },
  meta: {
    schema: metaSchema,
  },
});

const chConfig: CodeHikeConfig = {
  // optional (see code docs):
  components: { code: "Code" },
  // if you can't use RSC:
  syntaxHighlighting: {
    theme: "github-dark",
  },
};

export default defineConfig({
  lastModifiedTime: 'git',
  mdxOptions: {
    // MDX options
    remarkPlugins: [remarkMath, [remarkCodeHike, chConfig]],
    rehypePlugins: (v) => [rehypeKatex, ...v],
    recmaPlugins: [[recmaCodeHike, chConfig]],
  },
});


