import type { ReactNode } from 'react';
import { HomeLayout, HomeLayoutProps } from 'fumadocs-ui/layouts/home';
import { baseOptions } from '@/app/layout.config';
import { NarxiaIcon } from "@/lib/assets";
import Link from 'fumadocs-core/link';

const layoutProps: HomeLayoutProps = {
  ...baseOptions,
  nav: {
    title: (
      <>
        <NarxiaIcon width="32" height="32" />
        The Narxia Project
      </>
    ),
    enableHoverToOpen: true,
    transparentMode: 'always',
  },
  links: [
    { type: 'main', text: 'Users documentation', url: '/docs/user/' },
    { type: 'main', text: 'Developer documentation', url: '/docs/dev/' },
    ...(baseOptions.links ?? [])
  ]
}

export default function Layout({ children }: { children: ReactNode }) {
  return <HomeLayout {...layoutProps}>{children}</HomeLayout>;
}
