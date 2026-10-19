import { inject, provide, ref, type InjectionKey, type Ref } from 'vue'

/** Debug mode (toggled with D): controls on the board that change the game by hand. */
export interface Debug {
  active: Ref<boolean>
  /** Run an engine message, e.g. `{ tag: 'RaiseThreat', contents: [playerId, 1] }`. */
  send: (message: object) => void
}

const key: InjectionKey<Debug> = Symbol('debug')

export const provideDebug = (send: Debug['send']) => {
  const debug = { active: ref(false), send }
  provide(key, debug)
  return debug
}

export const useDebug = () => inject(key, { active: ref(false), send: () => {} })
