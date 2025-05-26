import type {BaseLayoutProps} from 'fumadocs-ui/layouts/shared';
import {NarxiaIcon, ZulipIcon} from "@/lib/assets";

/**
 * Shared layout configurations
 *
 * you can customise layouts individually from:
 * Home Layout: app/(home)/layout.tsx
 * Docs Layout: app/docs/layout.tsx
 */
export const baseOptions: BaseLayoutProps = {
    nav: {
        title: (
            <>
                <NarxiaIcon
                    width="32"
                    height="32"
                />
                The Narxia Project
            </>
        ),
    },
    // see https://fumadocs.dev/docs/ui/navigation/links
    links: [
        {
            type: 'icon',
            icon: <ZulipIcon width={"32px"} height={"32px"}/>,
            text: 'Narxia Zulip',
            url: 'https://nrx.zulipchat.com',
        }
    ],
    githubUrl: 'https://github.com/dnbln/narxia/tree/trunk/doc/docs',
};
