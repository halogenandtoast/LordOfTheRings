import { computed, ref, watch } from 'vue'
import { fetchBugs, type BugSummary } from './api'
import { currentUser } from './auth'

/**
 * The signed-in user's bug reports, for the bar: fetched as they sign in and every
 * minute after, which is about as often as the server sweeps GitHub.
 */
export const bugs = ref<BugSummary[]>([])

/** The reports the bar still shows: not cleared, or cleared and not yet seen. */
export const trackedBugs = computed(() => bugs.value.filter((bug) => bug.tracked))
/** Something has changed on one of them that the reporter has not seen. */
export const bugNews = computed(() => trackedBugs.value.some((bug) => bug.unread))

export const refreshBugs = async () => {
  if (!currentUser.value) {
    bugs.value = []
    return
  }
  try {
    bugs.value = await fetchBugs()
  } catch {
    // keep what was shown; the next refresh tries again
  }
}

const POLL_MS = 60_000
let timer: ReturnType<typeof setInterval> | null = null

watch(
  () => currentUser.value?.id,
  (id) => {
    if (timer) clearInterval(timer)
    timer = null
    void refreshBugs()
    if (id !== undefined) timer = setInterval(() => void refreshBugs(), POLL_MS)
  },
  { immediate: true },
)

/** How a report's status reads: a closed one's reason, or the board's column. */
export const statusKey = (status: string): { key: string; column?: string } => {
  if (status.startsWith('closed:')) {
    const reason = status.slice('closed:'.length)
    return { key: ['COMPLETED', 'NOT_PLANNED', 'DUPLICATE'].includes(reason) ? `bugs.closed.${reason}` : 'bugs.closed.COMPLETED' }
  }
  if (status.startsWith('board:') && status.length > 'board:'.length) return { key: 'bugs.column', column: status.slice('board:'.length) }
  return { key: 'bugs.filed' }
}
