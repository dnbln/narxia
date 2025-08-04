import { type NextRequest, NextResponse } from 'next/server';
import { getLLMText } from '@/lib/get-llm-text';
import { notFound } from 'next/navigation';
import { source } from "@/lib/source";

export const revalidate = false;

export async function GET(
    _req: NextRequest,
    { params }: { params: Promise<{ slug?: string[] }> },
) {
    const { slug } = await params;
    const oldSlug = slug?.slice(0, -1).concat(slug[slug.length - 1].replace('.mdx', ''));
    const page = source.getPage(oldSlug);
    if (!page) notFound();

    return new NextResponse(await getLLMText(page));
}

export function generateStaticParams() {
    return source.generateParams().map((params) => {
        return {
            ...params,
            slug: params.slug.slice(0, -1).concat(params.slug[params.slug.length - 1] + '.mdx'),
        }
    });
}