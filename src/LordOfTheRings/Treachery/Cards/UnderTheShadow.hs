module LordOfTheRings.Treachery.Cards.UnderTheShadow (underTheShadow) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype UnderTheShadow = UnderTheShadow TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

underTheShadow :: TreacheryCard UnderTheShadow
underTheShadow = treachery UnderTheShadow Cards.underTheShadow

-- | When Revealed: Until the end of the phase, raise the total threat in the staging area
-- by X, where X is the number of players in the game.
-- Shadow: Defending player raises his threat by the number of enemies with which he is
-- engaged.
instance RunMessage UnderTheShadow where
  runMessage msg t@(UnderTheShadow attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      players <- length <$> getPlayerOrder
      push $ CreateEffect $ Effect (toSource attrs) GameTarget [StagingThreatBonus players] UntilEndOfPhase
      pure t
    ResolveShadowEffect cid _ pid _ | cid == attrs.cardId -> do
      engaged <- length <$> select (EnemyEngagedWith $ PlayerWithId pid)
      pushWhen (engaged > 0) $ RaiseThreat pid engaged
      pure t
    _ -> UnderTheShadow <$> runMessage msg attrs
