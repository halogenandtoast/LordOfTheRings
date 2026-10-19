/**
 * Presentation preferences that outlive a page load.
 */
import { defineStore } from './store'
import { useStorage } from '@vueuse/core'

export type ViewMode = 'list' | 'grid'

export const useUiStore = defineStore('ui', () => {
  const viewMode = useStorage<ViewMode>('lotr:view', 'list')

  /** The product browser on the left and the facet panel on the right. */
  // shut at first on a screen too narrow for them beside the results (see BrowseView)
  const wide = !matchMedia('(width < 64rem)').matches
  const productsOpen = useStorage('lotr:products', wide)
  const filtersOpen = useStorage('lotr:filters', wide)

  /** Which cycles are expanded in the product browser. */
  const expandedCycles = useStorage<string[]>('lotr:cycles-open', [])

  function toggleCycle(code: string): void {
    const i = expandedCycles.value.indexOf(code)
    if (i === -1) expandedCycles.value.push(code)
    else expandedCycles.value.splice(i, 1)
  }

  function isCycleExpanded(code: string): boolean {
    return expandedCycles.value.includes(code)
  }

  return {
    viewMode,
    productsOpen,
    filtersOpen,
    expandedCycles,
    toggleCycle,
    isCycleExpanded,
  }
})
