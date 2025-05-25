import { Footer, Layout, Navbar } from 'nextra-theme-docs'
import {
    // Banner,
    Head
} from 'nextra/components'
import { getPageMap } from 'nextra/page-map'
import 'nextra-theme-docs/style.css'
import { ReactNode } from "react";
import './globals.css'
import narxiaLogo from '../../../logo/narxia.png'
import ZulipLogo from './zulip-icon.svg'

export const metadata = {
    // Define your metadata here
    // For more information on metadata API, see: https://nextjs.org/docs/app/building-your-application/optimizing/metadata
}

// const banner = <Banner storageKey="some-key">Nextra 4.0 is released 🎉</Banner>
const navbar = (
    <Navbar
        logo={<b className={'flex flex-row justify-center align-middle'} style={{ alignItems: 'center' }}><img src={narxiaLogo.src} height='64px' width='64px' alt='narxia logo' /> The Narxia Programming Language</b>}
        projectLink={'https://github.com/dnbln/narxia'}
        chatLink={'https://nrx.zulipchat.com'}
        chatIcon={<ZulipLogo width='32px' height='32px'/>}
    // ... Your additional navbar options
    />
)
const footer = <Footer>Copyright {new Date().getFullYear()} © Dinu Blanovschi.<br />Licensed under CC 4.0 BY-SA-NC</Footer>

export default async function RootLayout({ children }: { children: ReactNode }) {
    return (
        <html
            // Not required, but good for SEO
            lang="en"
            // Required to be set
            dir="ltr"
            // Suggested by `next-themes` package https://github.com/pacocoursey/next-themes#with-app
            suppressHydrationWarning
        >
            <Head
            // ... Your additional head options

            >
                {/* Your additional tags should be passed as `children` of `<Head>` element */}
            </Head>
            <body>
                <Layout
                    // banner={banner}
                    navbar={navbar}
                    pageMap={await getPageMap()}
                    docsRepositoryBase="https://github.com/dnbln/narxia/edit/trunk/doc/docs"
                    footer={footer}
                    feedback={{
                        link: 'https://nrx.zulipchat.com/'
                    }}
                // ... Your additional layout options
                >
                    {children}
                </Layout>
            </body>
        </html>
    )
}