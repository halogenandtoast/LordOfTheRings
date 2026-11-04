module LordOfTheRings.Treachery.Cards.BloodRunsChill (bloodRunsChill) where

import LordOfTheRings.Scenario.Scenarios.ThePassingOfTheGreyCompany.Helpers (hasThreatAtLeast35)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype BloodRunsChill = BloodRunsChill TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Doomed 1.
bloodRunsChill :: TreacheryCard BloodRunsChill
bloodRunsChill = treachery BloodRunsChill Cards.bloodRunsChill

-- | When Revealed: Each player with threat 35 or higher deals 1 damage to each exhausted
-- character he controls.
-- Shadow: If your threat is 35 or higher, defending character cannot ready until the end of
-- the round.
instance RunMessage BloodRunsChill where
  runMessage msg t@(BloodRunsChill attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer \p -> whenM (hasThreatAtLeast35 p) do
        exhausted <- selectCharacters (ExhaustedHero <> HeroControlledBy (PlayerWithId p)) (ExhaustedAlly <> AllyControlledBy (PlayerWithId p))
        for_ exhausted \c -> dealDamage attrs c 1
      pure t
    ResolveShadowEffect cid _ pid defenders | cid == attrs.cardId -> do
      whenM (hasThreatAtLeast35 pid) $ for_ defenders \d -> roundModifier attrs d CannotReady
      pure t
    _ -> BloodRunsChill <$> runMessage msg attrs
