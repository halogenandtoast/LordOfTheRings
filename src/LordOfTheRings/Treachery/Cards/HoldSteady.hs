module LordOfTheRings.Treachery.Cards.HoldSteady (holdSteady) where

import LordOfTheRings.Helpers.Character (getDefense)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype HoldSteady = HoldSteady TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

holdSteady :: TreacheryCard HoldSteady
holdSteady = treachery HoldSteady Cards.holdSteady

-- | When Revealed: Exhaust each character with 1 or less defense.
-- Shadow: If the defending character has 0 defense, discard it. Otherwise, the defending
-- character gets -2 defense for this attack.
instance RunMessage HoldSteady where
  runMessage msg t@(HoldSteady attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      characters <- selectCharacters ReadyHero ReadyAlly
      for_ characters \c -> whenM ((<= 1) <$> getDefense c) $ exhaust c
      pure t
    ResolveShadowEffect cid _ _ defenders | cid == attrs.cardId -> do
      for_ defenders \d -> do
        defense <- getDefense d
        if defense == 0 then discard attrs d else attackModifier attrs d (DefenseModifier (-2))
      pure t
    _ -> HoldSteady <$> runMessage msg attrs
