module LordOfTheRings.Location.Cards.ConcealedWoods (concealedWoods) where

import LordOfTheRings.Keyword (Keyword (Ambush, Surge))
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype ConcealedWoods = ConcealedWoods LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

concealedWoods :: LocationCard ConcealedWoods
concealedWoods = location ConcealedWoods Cards.concealedWoods

-- | While Concealed Woods is the active location, the first enemy with the ambush keyword
-- revealed from the encounter deck each round gains surge.
instance HasAbilities ConcealedWoods where
  getAbilities (ConcealedWoods a) =
    [ limited (GroupLimit PerRound 1)
        $ restricted a 1 (exists $ LocationWithId a.id <> IsActiveLocation)
        $ silent
        $ MatchingEnemyRevealedWindow When (EnemyWithKeyword Ambush)
    ]

-- | Shadow: Attacking enemy gets +2 [Attack] if engaged with the first player.
instance RunMessage ConcealedWoods where
  runMessage msg l@(ConcealedWoods a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 ws -> do
      for_ [eid | Window _ (Revealed (EnemyTarget eid)) <- ws] \eid -> roundModifier a eid (GainKeyword Surge)
      pure l
    ResolveShadowEffect cid eid pid _ | cid == a.cardId -> do
      lead <- getLead
      when (pid == lead) $ enemyGetsAttack a eid 2
      pure l
    _ -> ConcealedWoods <$> runMessage msg a
