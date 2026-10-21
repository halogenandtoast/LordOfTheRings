import { useStorage } from '@vueuse/core'

// What the board is drawn on, chosen in settings and kept in this browser: the plain page,
// or the TTS mod's playmat or felt.
export const tables = ['plain', 'playmat', 'felt'] as const
export type Table = (typeof tables)[number]
export const table = useStorage<Table>('lotr:table', 'plain')
