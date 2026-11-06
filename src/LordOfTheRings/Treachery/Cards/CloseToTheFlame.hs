module LordOfTheRings.Treachery.Cards.CloseToTheFlame (closeToTheFlame) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheBattleOfLakeTown.Shadow (offerHeroSacrifice)

newtype CloseToTheFlame = CloseToTheFlame TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

closeToTheFlame :: TreacheryCard CloseToTheFlame
closeToTheFlame = treachery CloseToTheFlame Cards.closeToTheFlame

instance RunMessage CloseToTheFlame where
  runMessage msg t@(CloseToTheFlame a) = case msg of
    WhenRevealed (isTarget a -> True) -> t <$ (pushAll =<< resolve (toSource a))
    ResolveShadowEffect cid _ _ _ | cid == a.cardId -> do
      players <- getPlayerOrder
      effects <- resolve $ CardIdSource cid
      offerHeroSacrifice (CardIdSource cid) (CardIdTarget cid) players effects
      pure t
    _ -> CloseToTheFlame <$> runMessage msg a
   where
    resolve source = map (\target -> DealDamage source target 1) <$> selectCharacters AnyHero AnyAlly
