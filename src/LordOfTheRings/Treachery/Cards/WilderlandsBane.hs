module LordOfTheRings.Treachery.Cards.WilderlandsBane (wilderlandsBane) where

import LordOfTheRings.Scenario.Scenarios.MountGundabad.Helpers
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype WilderlandsBane = WilderlandsBane TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

wilderlandsBane :: TreacheryCard WilderlandsBane
wilderlandsBane = treachery WilderlandsBane Cards.wilderlandsBane

-- | When Revealed: Dagnir makes an immediate attack against each engaged player. If no
-- attacks were made this way, Dagnir gets -30 engagement cost until the end of the round.
-- Shadow: Attacking enemy makes an additional attack against you after this one.
instance RunMessage WilderlandsBane where
  runMessage msg t@(WilderlandsBane attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      getDagnir >>= traverse_ \dagnir -> do
        engaged <- filterM (\p -> selectAny (EnemyWithId dagnir <> EnemyEngagedWith (PlayerWithId p))) =<< getRemainingPlayers
        if null engaged
          then roundModifier attrs dagnir (EngagementCostModifier (-30))
          else for_ engaged \p -> immediateAttack p dagnir
      pure t
    ResolveShadowEffect cid eid pid _ | cid == attrs.cardId -> do
      additionalAttack pid eid
      pure t
    _ -> WilderlandsBane <$> runMessage msg attrs
