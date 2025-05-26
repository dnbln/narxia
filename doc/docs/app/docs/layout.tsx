import { DocsLayout, DocsLayoutProps } from 'fumadocs-ui/layouts/docs';
import type { ReactNode } from 'react';
import { baseOptions } from '@/app/layout.config';
import { source } from '@/lib/source';
// import { GithubInfo } from 'fumadocs-ui/components/github-info';
// import {ZulipIcon} from "@/lib/assets";

const docsOptions: DocsLayoutProps = {
  ...baseOptions,
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
