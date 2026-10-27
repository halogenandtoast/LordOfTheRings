module LordOfTheRings.Location.Cards.KneesOfTheMountain (kneesOfTheMountain) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype KneesOfTheMountain = KneesOfTheMountain LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

kneesOfTheMountain :: LocationCard KneesOfTheMountain
kneesOfTheMountain = location KneesOfTheMountain Cards.kneesOfTheMountain

-- | While Knees of the Mountain is in the staging area, it gains: "Forced: After an enemy
-- engages a player, it gets +1 [Attack] until the end of the round."
instance HasAbilities KneesOfTheMountain where
  getAbilities (KneesOfTheMountain a) =
    [ restricted a 1 (exists $ LocationWithId a.id <> LocationInStagingArea)
        $ forced
        $ AnyEnemyEngagedWindow After
    ]

instance RunMessage KneesOfTheMountain where
  runMessage msg l@(KneesOfTheMountain a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 ws -> do
      for_ [eid | Window _ (EnemyEngaged eid _) <- ws] \eid -> roundModifier a eid (AttackModifier 1)
      pure l
    _ -> KneesOfTheMountain <$> runMessage msg a
