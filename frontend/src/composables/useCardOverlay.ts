import { computed, reactive, readonly, ref, shallowRef } from 'vue'
import type { CardView } from '../types'

const OVERLAY_WIDTH = 300
const CARD_RATIO = 0.715
const GAP = 10
const PAD = 10

interface Placement {
  top: number
  left: number
  width: number
  height: number
}

const card = shallowRef<CardView | null>(null)
const sideways = ref(false)
const anchor = shallowRef<HTMLElement | null>(null)
const placement = reactive<Placement>({ top: 0, left: 0, width: OVERLAY_WIDTH, height: 0 })

let frame: number | null = null
let listening = false

const measure = () => {
  const element = anchor.value
  if (!element) return

  const rect = element.getBoundingClientRect()
  const width = sideways.value ? OVERLAY_WIDTH / CARD_RATIO : OVERLAY_WIDTH
  const height = sideways.value ? OVERLAY_WIDTH : Math.round(OVERLAY_WIDTH / CARD_RATIO)

  // Prefer the right of the card, flipping to its left when that would run off screen.
  const toTheRight = rect.right + GAP
  const left =
    toTheRight + width + PAD >= window.innerWidth ? rect.left - width - GAP : toTheRight

  // Line up with the card, then lift it so the whole card stays inside the viewport.
  const desiredTop = rect.top - 40
  const maxTop = window.innerHeight - height - PAD
  const top = Math.max(PAD, Math.min(desiredTop, maxTop))

  placement.top = top
  placement.left = Math.max(PAD, Math.min(left, window.innerWidth - width - PAD))
  placement.width = width
  placement.height = height
}

const schedule = () => {
  if (frame !== null) cancelAnimationFrame(frame)
  frame = requestAnimationFrame(() => {
    frame = null
    measure()
  })
}

const listen = () => {
  if (listening) return
  listening = true
  window.addEventListener('scroll', schedule, true)
  window.addEventListener('resize', schedule)
}

export const useCardOverlay = () => ({
  card: readonly(card),
  sideways: readonly(sideways),
  placement,
  visible: computed(() => card.value !== null),
  show: (shown: CardView, element: HTMLElement, rotated = false) => {
    listen()
    card.value = shown
    sideways.value = rotated
    anchor.value = element
    measure()
  },
  hide: () => {
    card.value = null
    anchor.value = null
  },
})
