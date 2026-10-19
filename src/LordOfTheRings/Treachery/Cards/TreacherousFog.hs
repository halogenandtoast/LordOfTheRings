module LordOfTheRings.Treachery.Cards.TreacherousFog (treacherousFog) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype TreacherousFog = TreacherousFog TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

treacherousFog :: TreacheryCard TreacherousFog
treacherousFog = treachery TreacherousFog Cards.treacherousFog

-- | When Revealed: Each location in the staging area gets +1 threat until the end of the
-- phase. Then, each player with a threat of 35 or higher chooses and discards 1 card from
-- his hand.
instance RunMessage TreacherousFog where
  runMessage msg t@(TreacherousFog attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      locations <- select LocationInStagingArea
      order <- getPlayerOrder
      high <- filterM (fmap ((>= 35) . (.threat)) . getPlayer) order
      pushAll
        $ [CreateEffect $ Effect (toSource attrs) (toTarget l) [ThreatModifier 1] UntilEndOfPhase | l <- locations]
        <> [ChooseAndDiscardFromHand p (toSource attrs) 1 | p <- high]
      pure t
    _ -> TreacherousFog <$> runMessage msg attrs
