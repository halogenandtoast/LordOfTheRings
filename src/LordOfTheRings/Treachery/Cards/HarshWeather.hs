module LordOfTheRings.Treachery.Cards.HarshWeather (harshWeather) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype HarshWeather = HarshWeather TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

harshWeather :: TreacheryCard HarshWeather
harshWeather = treachery HarshWeather Cards.harshWeather

-- | When Revealed: For each ready character a player controls, he must choose: either
-- exhaust that character or raise his threat by 1.
-- Shadow: Attacking enemy gets +1 attack. If this attack destroys a character, exhaust a
-- character you control.
instance RunMessage HarshWeather where
  runMessage msg t@(HarshWeather attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer \p -> do
        readied <- selectCharacters (HeroControlledBy (PlayerWithId p) <> ReadyHero) (AllyControlledBy (PlayerWithId p) <> ReadyAlly)
        for_ readied \c -> chooseOneM p do
          questionLabeled "harshWeather.choose"
          causedBy attrs
          aimedAt [c]
          labeled "harshWeather.exhaust" $ exhaust c
          labeled "harshWeather.raiseThreat" $ encounterRaisesThreat p 1
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      enemyGetsAttack (CardIdSource cid) eid 1
      ifThisAttackDestroys eid msg
      pure t
    DoStep 1 (ResolveShadowEffect cid _ pid _) | cid == attrs.cardId -> do
      readied <- selectCharacters (HeroControlledBy (PlayerWithId pid) <> ReadyHero) (AllyControlledBy (PlayerWithId pid) <> ReadyAlly)
      chooseOneM pid do
        questionLabeled "harshWeather.exhaustOne"
        causedBy attrs
        targets readied exhaust
      pure t
    _ -> HarshWeather <$> runMessage msg attrs
