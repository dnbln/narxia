import type { ReactNode } from 'react';
import {HomeLayout, HomeLayoutProps} from 'fumadocs-ui/layouts/home';
import { baseOptions } from '@/app/layout.config';
import {NarxiaIcon} from "@/lib/assets";

const layoutProps: HomeLayoutProps = {
  ...baseOptions,
  nav: {
    title: (
        <>
          <NarxiaIcon width="64" height="64" />
          The Narxia Project
        </>
    )
  }
}

export default function Layout({ children }: { children: ReactNode }) {
  return <HomeLayout {...layoutProps}>{children}</HomeLayout>;
}
