module LordOfTheRings.Treachery.Cards.ScorchingBlast (scorchingBlast) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheBattleOfLakeTown.Shadow (offerHeroSacrifice)

newtype ScorchingBlast = ScorchingBlast TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

scorchingBlast :: TreacheryCard ScorchingBlast
scorchingBlast = treachery ScorchingBlast Cards.scorchingBlast

instance RunMessage ScorchingBlast where
  runMessage msg t@(ScorchingBlast a) = case msg of
    WhenRevealed (isTarget a -> True) -> t <$ (pushAll =<< resolve (toSource a))
    ResolveShadowEffect cid _ _ _ | cid == a.cardId -> do
      players <- getPlayerOrder
      effects <- resolve $ CardIdSource cid
      offerHeroSacrifice (CardIdSource cid) (CardIdTarget cid) players effects
      pure t
    _ -> ScorchingBlast <$> runMessage msg a
   where
    resolve source = pure [ResolveBurnDamage source]
