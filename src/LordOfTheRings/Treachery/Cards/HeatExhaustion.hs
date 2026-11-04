module LordOfTheRings.Treachery.Cards.HeatExhaustion (heatExhaustion) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype HeatExhaustion = HeatExhaustion TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

heatExhaustion :: TreacheryCard HeatExhaustion
heatExhaustion = treachery HeatExhaustion Cards.heatExhaustion

-- | When Revealed: Each player exhausts a character he controls and deals 1 damage to that
-- character: a ready one, as only it can be exhausted; with none, nothing happens.
-- Shadow: Exhaust a character you control.
instance RunMessage HeatExhaustion where
  runMessage msg t@(HeatExhaustion attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer \p -> do
        let you = PlayerWithId p
        ready' <- selectCharacters (HeroControlledBy you <> ReadyHero) (AllyControlledBy you <> ReadyAlly)
        chooseOrRunOneM p do
          questionLabeled "heatExhaustion.exhaust"
          causedBy attrs
          targets ready' \c -> do
            exhaust c
            dealDamage attrs c 1
      pure t
    ResolveShadowEffect cid _ pid _ | cid == attrs.cardId -> do
      chooseAndExhaust attrs pid 1
      pure t
    _ -> HeatExhaustion <$> runMessage msg attrs
