import { DocsLayout, DocsLayoutProps } from 'fumadocs-ui/layouts/docs';
import type { ReactNode } from 'react';
import { baseOptions } from '@/app/layout.config';
import { source } from '@/lib/source';
// import { NarxiaIcon } from '@/lib/assets';
// import { GithubInfo } from 'fumadocs-ui/components/github-info';
// import {ZulipIcon} from "@/lib/assets";

const docsOptions: DocsLayoutProps = {
  ...baseOptions,
  nav: {
    title: (
      <>
        The Narxia Project
      </>
    ),
  },
  tree: source.pageTree,
  // sidebar: {
  //   title: 'Docs',
  //   tabs: [
  //
  //   ]
  // }
};

export default function Layout({ children }: { children: ReactNode }) {
  return (
    <DocsLayout {...docsOptions} >
      {children}
    </DocsLayout>
  );
}
