module LordOfTheRings.Ally.Cards.Landroval (landroval) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Card (Card (PlayerCard), PlayerCard (..))
import LordOfTheRings.Placement (placementController)

newtype Landroval = Landroval AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

landroval :: AllyCard Landroval
landroval = ally Landroval Cards.landroval

-- | Sentinel. Landroval cannot have restricted attachments.
-- Response: After a hero card is destroyed, return Landroval to his owner's hand to put
-- that hero back into play, with 1 damage token on it. (Limit once per game.)
instance HasAbilities Landroval where
  getAbilities (Landroval a) = [limited (PlayerLimit PerGame 1) $ mkAbility a 1 $ response $ HeroDestroyedWindow After]

instance RunMessage Landroval where
  runMessage msg s@(Landroval a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 windows -> do
      returnToHand a
      for_ [c | Window _ (CharacterDestroyed (HeroTarget _) (PlayerCard c)) <- windows] \c ->
        for_ (c.owner <|> placementController a.placement) \owner -> do
          removeFromDiscard owner c.pcId
          putHeroIntoPlay owner c
          placeTokens a c Damage 1
      pure s
    _ -> Landroval <$> runMessage msg a
