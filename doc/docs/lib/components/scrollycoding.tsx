"use client"

import { z } from "zod"
import {
    Selection,
    Selectable,
    SelectionProvider,
} from "codehike/utils/selection"
import { Block, HighlightedCodeBlock, parseProps } from "codehike/blocks"
import { HighlightedCode, Pre } from "codehike/code"

import { tokenTransitions } from "./annotations/token-transitions"
import { wordWrap } from "./annotations/word-wrap"
import { mark } from "./annotations/mark"
import { tooltip } from "./annotations/tooltip"
import { ReactNode } from "react"
import { collapse, collapseTrigger, collapseContent } from "./annotations/collapsible"

const Schema = Block.extend({
    steps: z.array(Block.extend({ code: HighlightedCodeBlock })),
    tooltips: z.array(Block).optional(),
    doctooltips: z.array(Block).optional(),
})

export function ScrollyCoding(props: unknown) {
    const { steps, tooltips, doctooltips } = parseProps(props, Schema)
    const compiledTooltips = (tooltips ?? []).concat(doctooltips ?? [])
    // console.log("ScrollyCoding props", steps, tooltips, doctooltips)

    return (
        <SelectionProvider className="flex gap-4">
            <div className="flex-1 mt-32 mb-[90vh] ml-2 prose prose-invert">
                {steps.map((step, i) => (
                    <Selectable
                        key={i}
                        index={i}
                        selectOn={["click", "scroll"]}
                        className="border-l-4 border-zinc-700 data-[selected=true]:border-blue-400 px-5 py-2 mb-24 rounded bg-(--scrollycoding-steps)"
                    >
                        <h2 className="mt-4 text-xl">{step.title}</h2>
                        <div>{step.children}</div>
                    </Selectable>
                ))}
            </div>
            <div className="w-[40vw] max-w-xl bg-zinc-900">
                <div className="top-16 sticky overflow-auto">
                    <Selection
                        from={steps.map((step) => (
                            <Code codeblock={step.code} tooltips={compiledTooltips} key={0} />
                        ))}
                    />
                </div>
            </div>
        </SelectionProvider>
    )
}

function Code({ codeblock, tooltips }: { codeblock: HighlightedCode, tooltips: { title?: string, children?: ReactNode }[] | undefined }) {
    codeblock.annotations = codeblock.annotations.map((a) => {
        // console.log("Query", a.query)
        // console.log("Titles", tooltips?.map((t) => t.title))
        const tooltip = tooltips?.find((t) => t.title === a.query)
        if (!tooltip) return a
        return {
            ...a,
            data: { ...a.data, children: tooltip.children },

        }
    })

    return (
        <Pre
            code={codeblock}
            handlers={[
                collapse,
                collapseTrigger,
                collapseContent,
                tokenTransitions,
                wordWrap,
                mark,
                tooltip,
            ]}
            className="min-h-[40rem] p-3"
        />
    )
}