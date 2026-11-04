module LordOfTheRings.Event.Cards.UnlikelyFriendship (unlikelyFriendship) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Trait (Trait (Dwarf, Silvan))

newtype UnlikelyFriendship = UnlikelyFriendship EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Play only if you control a unique character with the Silvan trait and another unique
-- character with the Dwarf trait.
unlikelyFriendship :: EventCard UnlikelyFriendship
unlikelyFriendship =
  eventWith UnlikelyFriendship Cards.unlikelyFriendship \a ->
    let you = PlayerWithId a.owner
        unique trait = (HeroControlledBy you <> HeroWithTrait trait, AllyControlledBy you <> UniqueAlly <> AllyWithTrait trait)
     in a & criteriaL .~ DistinctCharacters [unique Silvan, unique Dwarf]

-- | Action: Draw 1 card and add 1 resource to the resource pool of a hero you control.
instance RunMessage UnlikelyFriendship where
  runMessage msg e@(UnlikelyFriendship attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      drawCards attrs pid 1
      heroes <- select (HeroControlledBy $ PlayerWithId pid)
      chooseOrRunOneM pid do
        questionLabeled "chooseHeroForResource"
        causedBy attrs
        targets heroes \h -> placeResources attrs h 1
      pure e
    _ -> UnlikelyFriendship <$> runMessage msg attrs
