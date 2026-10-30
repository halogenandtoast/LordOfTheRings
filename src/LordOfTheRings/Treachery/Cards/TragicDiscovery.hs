module LordOfTheRings.Treachery.Cards.TragicDiscovery (tragicDiscovery) where

import LordOfTheRings.Helpers.Staging (getLocationThreat)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype TragicDiscovery = TragicDiscovery TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

tragicDiscovery :: TreacheryCard TragicDiscovery
tragicDiscovery = treachery TragicDiscovery Cards.tragicDiscovery

-- | When Revealed: Raise each player's threat by the threat of the active location.
-- Shadow: Defending player either raises his threat by 3, or this attack is considered
-- undefended. An undefended attack already is, so it asks nothing.
instance RunMessage TragicDiscovery where
  runMessage msg t@(TragicDiscovery attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      threat <- sum <$> (traverse getLocationThreat =<< select IsActiveLocation)
      eachPlayer \p -> encounterRaisesThreat p threat
      pure t
    ResolveShadowEffect cid _ pid defenders | cid == attrs.cardId -> do
      unless (null defenders) do
        chooseOneM pid do
          questionLabeled "tragicDiscovery.choose"
          causedBy attrs
          labeled "tragicDiscovery.raiseThreat" $ encounterRaisesThreat pid 3
          -- the attack's own messages are on the game's queue once the choice is made
          labeled "tragicDiscovery.undefended" $ doStep 1 msg
      pure t
    DoStep 1 (ResolveShadowEffect cid eid pid _) | cid == attrs.cardId -> do
      makeUndefended pid eid
      pure t
    _ -> TragicDiscovery <$> runMessage msg attrs
