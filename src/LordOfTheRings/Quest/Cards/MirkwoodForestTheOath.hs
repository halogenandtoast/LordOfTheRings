module LordOfTheRings.Quest.Cards.MirkwoodForestTheOath (mirkwoodForestTheOath) where

import LordOfTheRings.Card (CardType (LocationType))
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Trait (Trait (Forest))

newtype MirkwoodForestTheOath = MirkwoodForestTheOath QuestAttrs
  deriving anyclass (IsQuest, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

mirkwoodForestTheOath :: QuestCard MirkwoodForestTheOath
mirkwoodForestTheOath = quest MirkwoodForestTheOath Cards.mirkwoodForestTheOath

instance HasModifiersFor MirkwoodForestTheOath where
  getModifiersFor (MirkwoodForestTheOath a) = when a.flipped do
    trail <-
      getsGame
        (any (cardMatches $ cardIs Locations.goblinTrail) . (.victoryDisplay) . toAttrs . (.scenario))
    unless trail $ modifySelf a [CannotBeDefeated]

instance RunMessage MirkwoodForestTheOath where
  runMessage msg q@(MirkwoodForestTheOath attrs) = case msg of
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      askToResolveWhenRevealed attrs
      MirkwoodForestTheOath <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer \pid -> searchEncounterDeckAndDiscard pid attrs (CardWithType LocationType <> CardWithTrait Forest)
      shuffleEncounterDeck
      pure q
    _ -> MirkwoodForestTheOath <$> runMessage msg attrs
