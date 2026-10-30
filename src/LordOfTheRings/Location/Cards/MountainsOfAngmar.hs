module LordOfTheRings.Location.Cards.MountainsOfAngmar (mountainsOfAngmar) where

import LordOfTheRings.Card (cdShadowEffect, toCardDef)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))

newtype MountainsOfAngmar = MountainsOfAngmar LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

mountainsOfAngmar :: LocationCard MountainsOfAngmar
mountainsOfAngmar = location MountainsOfAngmar Cards.mountainsOfAngmar

-- | While Mountains of Angmar is the active location, each shadow card with no shadow
-- effect gains "Shadow: Raise defending player's threat by 2."
instance HasModifiersFor MountainsOfAngmar where
  getModifiersFor (MountainsOfAngmar a) = when (a.placement == ActiveLocation) $ modified a GameTarget [ShadowlessCardsGainEffect]

-- | Shadow: Raise defending player's threat by 2. The gained effect resolves here, as each
-- shadow card without one of its own resolves.
instance RunMessage MountainsOfAngmar where
  runMessage msg l@(MountainsOfAngmar a) = case msg of
    ResolveShadowEffect cid _ pid _ | cid == a.cardId -> do
      encounterRaisesThreat pid 2
      pure l
    ResolveShadowCard cid eid pid _ | a.placement == ActiveLocation -> do
      dealt <- (.shadowCards) <$> getEnemy eid
      for_ (find ((== cid) . (.id)) dealt) \card ->
        unless (cdShadowEffect $ toCardDef card) $ encounterRaisesThreat pid 2
      pure l
    _ -> MountainsOfAngmar <$> runMessage msg a
