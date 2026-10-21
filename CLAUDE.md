# LordOfTheRings

The Haskell engine in `src/LordOfTheRings` follows `~/Code/ArkhamHorror`'s idioms. Look
up how Arkham does a mechanic before adding one.

## Writing cards

`runMessage` runs in `QueueT Message GameT`: each card has a queue of its own, and the
game puts what it pushed on the front of the game's queue once the card is done. So:

- Pushes keep the order they are written in. Write effects in the order the card text
  reads them. Don't build lists for `pushAll` just to get the order right.
- Fall through with `_ -> MyCard <$> runMessage msg attrs`. There's no `runQueueT` or
  `liftRunMessage` to write. The attrs share the card's queue, so their messages come
  after anything the card pushed before falling through.
- Push with the verbs in `LordOfTheRings.Message.Lifted`, such as `dealDamage`,
  `placeProgress`, `drawCards`, `raiseThreat`, `phaseModifier`, `eachPlayer`,
  `choosePlayerM`, `withHost` and `revealEncounterCard`. Use raw `push Msg` only when no
  helper fits. When the same push turns up in a second card, add a helper there.
- Build questions with `LordOfTheRings.Message.Lifted.Choose`. Each choice is an action,
  and the messages it pushes become that choice's:

  ```haskell
  chooseOneM pid do
    questionLabeled "gandalf.chooseOne" -- key under label. in frontend/src/locales/en/label.json
    causedBy attrs
    labeled "gandalf.draw" $ drawCards pid 3
    targets enemies \e -> dealDamage attrs e 4
  ```

  Nothing is asked when there are no choices. `chooseOrRunOneM` takes a lone choice
  without asking.
- A card's queue holds only what that card pushed. To cancel something already on the
  game's queue, use `cancelMatching` or `cancelWhenRevealed`. `popMessagesMatching` won't
  reach the game's queue.

## Commits

Make small, focused commits with no Co-Authored-By or other AI trailer. Other sessions
share this tree, so stage only your own changes.

## The game log

The log is derived from the message stream, as in Arkham: `LordOfTheRings.Log.Narrator`
says what each `Message` means, read before the message runs. Cards write nothing.

- To log something new, add a case to `narrationFor` for the message that does it, not a
  `sendLog` in a card. Leave plumbing silent, and never push or throw from a narration.
- Every English string is under `log.` in `frontend/src/locales/en/log.json`.
- An event that spans answers goes in a block: push its messages through `inLogBlock`
  with a new `LogBlockKind`, and give the kind a header in the narrator's `openBlock`.
