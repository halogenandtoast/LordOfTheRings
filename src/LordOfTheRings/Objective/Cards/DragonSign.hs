module LordOfTheRings.Objective.Cards.DragonSign (dragonSign) where

import LordOfTheRings.Card (toCard)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Game.Base (gameEntities)
import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted
import LordOfTheRings.Placement (Placement (AttachedTo))

newtype DragonSign = DragonSign ObjectiveAttrs
  deriving anyclass (IsObjective, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Guarded (enemy).
dragonSign :: ObjectiveCard DragonSign
dragonSign = objective DragonSign Cards.dragonSign

{- | Attached enemy or location is immune to player card effects: the enemy guarding it, or
the location it is attached to, as Creature Den attaches it.
When Dragon Sign is free of encounters, add it to the victory display: the game finds it
free as it would an objective the first player claims ('FirstPlayerClaimsWhenFree'), and it
goes to the victory display instead of to a hero.
-}
instance HasModifiersFor DragonSign where
  getModifiersFor (DragonSign a) = do
    modifySelf a [FirstPlayerClaimsWhenFree]
    case a.placement of
      AttachedTo host -> modified a host [ImmuneToPlayerCardEffects]
      _ -> pure ()
    for_ a.objectiveGuard \cid -> do
      es <- getsGame (toList . (.enemies) . gameEntities)
      for_ (filter ((== cid) . (.id) . toCard) es) \e -> modified a (toTarget e) [ImmuneToPlayerCardEffects]

instance RunMessage DragonSign where
  runMessage msg o@(DragonSign attrs) = case msg of
    ClaimFreeObjective oid | oid == attrs.objectiveId -> do
      addToVictoryDisplay attrs
      pure o
    _ -> DragonSign <$> runMessage msg attrs
