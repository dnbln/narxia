import { AnnotationHandler } from "codehike/code"
import {
  TooltipProvider,
  Tooltip,
  TooltipTrigger,
  TooltipContent,
} from "@/components/ui/tooltip"

export const tooltip: AnnotationHandler = {
  name: "tooltip",
  Inline: ({ children, annotation }) => {
    const { query, data } = annotation
    return (
      <TooltipProvider>
        <Tooltip>
          <TooltipTrigger className="underline underline-offset-4 decoration-white tooltip-trigger">
            {children}
          </TooltipTrigger>
          <TooltipContent align="center" className="max-w-3xl p-0">
            {data?.children || query}
          </TooltipContent>
        </Tooltip>
      </TooltipProvider>
    )
  },
}