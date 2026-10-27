module LordOfTheRings.Event.Cards.GraveCairn (graveCairn) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Card (cdAttack, toCardDef)

newtype GraveCairn = GraveCairn EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Response: After a character leaves play, add its attack to another character's attack
-- until the end of the round. Its printed attack: its modifiers left play with it.
graveCairn :: EventCard GraveCairn
graveCairn = eventWith GraveCairn Cards.graveCairn (windowL ?~ CharacterLeftPlayWindow After AnyCard)

instance RunMessage GraveCairn where
  runMessage msg e@(GraveCairn attrs) = case msg of
    PlayEvent pid card windows | card.id == toId attrs -> do
      for_ (take 1 [c | Window _ (CharacterLeftPlay c) <- windows]) \c -> do
        characters <- selectCharacters AnyHero AnyAlly
        chooseOneM pid do
          questionLabeled "graveCairn.choose"
          causedBy attrs
          targets characters \t -> roundModifier attrs t (AttackModifier $ fromMaybe 0 $ cdAttack $ toCardDef c)
      pure e
    _ -> GraveCairn <$> runMessage msg attrs
