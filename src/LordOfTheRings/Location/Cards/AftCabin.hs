module LordOfTheRings.Location.Cards.AftCabin (aftCabin) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (AttachedTo, StagingArea))
import LordOfTheRings.Trait (Trait (Deck))

newtype AftCabin = AftCabin LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

aftCabin :: LocationCard AftCabin
aftCabin = location AftCabin Cards.aftCabin

-- | While Aft Cabin is in the staging area, each enemy grappled with each Deck location adds
-- 3 threat instead of 2: 1 more on top of the grapple's 2, from the first copy staged alone.
instance HasModifiersFor AftCabin where
  getModifiersFor (AftCabin a) = when (a.placement == StagingArea) do
    cabins <- select (LocationWithCardCode (toCardCode Cards.aftCabin) <> LocationInStagingArea)
    when (minimumMay cabins == Just a.id) do
      decks <- select (LocationWithTrait Deck)
      for_ decks \l -> do
        n <- selectCount (EnemyGrappledWith (LocationWithId l))
        when (n > 0) $ modified a l [ThreatModifier n]

-- | Forced: After an enemy is detached from Aft Cabin, that enemy gets +3 attack until the
-- end of the round. A grappling enemy lets go for the staging area.
instance RunMessage AftCabin where
  runMessage msg (AftCabin a) = case msg of
    PlaceEnemy eid StagingArea -> do
      e <- getEnemy eid
      when (e.placement == AttachedTo (toTarget a)) $ roundModifier a eid (AttackModifier 3)
      AftCabin <$> runMessage msg a
    _ -> AftCabin <$> runMessage msg a
