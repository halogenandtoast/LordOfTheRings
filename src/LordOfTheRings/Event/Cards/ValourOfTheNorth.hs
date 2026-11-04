module LordOfTheRings.Event.Cards.ValourOfTheNorth (valourOfTheNorth) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted

newtype ValourOfTheNorth = ValourOfTheNorth EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Response: After a character is declared as an attacker or defender against an enemy
-- with an attachment, that character gets +3 attack and +3 defense until the end of this
-- attack.
valourOfTheNorth :: EventCard ValourOfTheNorth
valourOfTheNorth =
  eventWith ValourOfTheNorth Cards.valourOfTheNorth
    $ windowL
    ?~ AnyWindowOf
      [ CharacterAttackerDeclaredWindow After AnyHero AnyAlly EnemyWithAnyAttachment
      , CharacterDefenderDeclaredWindow After AnyHero AnyAlly EnemyWithAnyAttachment
      ]

instance RunMessage ValourOfTheNorth where
  runMessage msg e@(ValourOfTheNorth attrs) = case msg of
    PlayEvent pid card windows | card.id == toId attrs -> do
      let declared = [(c, eid) | Window After (DeclaredAsAttacker c eid) <- windows] <> [(c, eid) | Window After (DeclaredAsDefender c eid) <- windows]
      characters <- map fst <$> filterM (selectAny . (<> EnemyWithAnyAttachment) . EnemyWithId . snd) declared
      chooseOrRunOneM pid do
        questionLabeled "valourOfTheNorth.character"
        causedBy attrs
        targets (ordNub characters) \c -> createEffect attrs c [AttackModifier 3, DefenseModifier 3] UntilEndOfAttack
      pure e
    _ -> ValourOfTheNorth <$> runMessage msg attrs
