import {source} from '@/lib/source';
import {
    DocsPage,
    DocsBody,
    DocsDescription,
    DocsTitle,
} from 'fumadocs-ui/page';
import {notFound} from 'next/navigation';
import {createRelativeLink} from 'fumadocs-ui/mdx';
import {getMDXComponents} from '@/mdx-components';
import {LLMCopyButton, EditOnGitHub} from './page.client';

export default async function Page(props: {
    params: Promise<{ slug: string[] }>;
}) {
    const params = await props.params;
    const slug = params.slug;
    const page = source.getPage(slug);
    if (!page) notFound();

    const path = `doc/docs/content/docs/${page.file.path}`;
    const MDXContent = page.data.body;

    return (
        <DocsPage toc={page.data.toc}
                  full={page.data.full}
                  article={page.data.full ? {className: 'max-w-full'} : {}}
                  breadcrumb={{enabled: true}}
                  lastUpdate={page.data.lastModified}
                  tableOfContent={{style: 'clerk', single: false}}
                  tableOfContentPopover={{style: 'clerk'}}
        >
            <DocsTitle>{page.data.title}</DocsTitle>
            <DocsDescription>{page.data.description}</DocsDescription>
            <div className="flex flex-row gap-2 items-center mb-4">
                <LLMCopyButton slug={slug}/>
                <EditOnGitHub
                    url={`https://github.com/dnbln/narxia/blob/trunk/${path}`}
                />
            </div>
            <DocsBody>
                <MDXContent
                    components={getMDXComponents({
                        // this allows you to link to other pages with relative file paths
                        a: createRelativeLink(source, page),
                    })}
                />
            </DocsBody>
        </DocsPage>
    );
}

export async function generateStaticParams() {
    return source.generateParams();
}

export async function generateMetadata(props: {
    params: Promise<{ slug?: string[] }>;
}) {
    const params = await props.params;
    const page = source.getPage(params.slug);
    if (!page) notFound();

    return {
        title: page.data.title,
        description: page.data.description,
    };
}
