module LordOfTheRings.Hero.Cards.CirdanTheShipwright (cirdanTheShipwright) where

import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted
import LordOfTheRings.Player.Types (discardableHand)

newtype CirdanTheShipwright = CirdanTheShipwright HeroAttrs
  deriving anyclass (IsHero)
  deriving newtype (Show, Eq, ToJSON, Entity)

cirdanTheShipwright :: HeroCard CirdanTheShipwright
cirdanTheShipwright = hero CirdanTheShipwright Cards.cirdanTheShipwright

-- | Draw 1 additional card at the beginning of the resource phase.
instance HasModifiersFor CirdanTheShipwright where
  getModifiersFor (CirdanTheShipwright a) = modified a (PlayerTarget a.owner) [AdditionalResourcePhaseDraws 1]

-- | Forced: After drawing cards at the beginning of the resource phase, choose and discard 1
-- of those cards.
instance HasAbilities CirdanTheShipwright where
  getAbilities (CirdanTheShipwright a) = [mkAbility a 1 $ forced $ DrewForResourcePhaseWindow After a.owner]

instance RunMessage CirdanTheShipwright where
  runMessage msg h@(CirdanTheShipwright attrs) = case msg of
    UseCardAbility pid (isSource attrs -> True) 1 windows -> do
      let drawn = concat [cids | Window _ (DrewForResourcePhase _ cids) <- windows]
      cards <- filter ((`elem` drawn) . (.id)) . discardableHand <$> getPlayer pid
      chooseOrRunOneM pid do
        questionLabeled "cirdanTheShipwright.discard"
        causedBy attrs
        targets cards (discardFromHand pid)
      pure h
    _ -> CirdanTheShipwright <$> runMessage msg attrs
