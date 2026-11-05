module LordOfTheRings.Treachery.Cards.OrcDraught (orcDraught) where

import LordOfTheRings.Helpers.Modifiers (immuneToSource)
import LordOfTheRings.Scenario.Scenarios.TheUrukHai.Helpers (increasePursuit)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype OrcDraught = OrcDraught TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

orcDraught :: TreacheryCard OrcDraught
orcDraught = treachery OrcDraught Cards.orcDraught

instance RunMessage OrcDraught where
  runMessage msg t@(OrcDraught a) = case msg of
    WhenRevealed (isTarget a -> True) -> do
      you <- getRevealer
      allies <- filterM (fmap not . immuneToSource (toSource a)) =<< select (AllyControlledBy $ PlayerWithId you)
      chooseOrRunOneM you do
        questionLabeled "orcDraught.choose"
        causedBy a
        labeled "orcDraught.pursuit" $ increasePursuit 1
        when (notNull allies) $ labeled "orcDraught.discard" do
          chooseOrRunOneM you do
            questionLabeled "orcDraught.ally"
            causedBy a
            targets allies (discard a)
      pure t
    ResolveShadowEffect cid _ pid _ | cid == a.cardId -> do
      chooseAndExhaust a pid 1
      pure t
    _ -> OrcDraught <$> runMessage msg a
