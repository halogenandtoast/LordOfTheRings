module LordOfTheRings.Enemy.Cards.SoullessCadaver (soullessCadaver) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (Placement (EngagedWith))

newtype SoullessCadaver = SoullessCadaver EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, Entity)

soullessCadaver :: EnemyCard SoullessCadaver
soullessCadaver = enemy SoullessCadaver Cards.soullessCadaver

-- | While you are engaged with Soulless Cadaver, your deck cannot be searched.
instance HasModifiersFor SoullessCadaver where
  getModifiersFor (SoullessCadaver a) = case a.placement of
    EngagedWith pid -> modified a (PlayerTarget pid) [CannotSearchDeck]
    _ -> pure ()

-- | Forced: When Soulless Cadaver engages you, place each card in your hand with printed
-- cost 2 or less on the bottom of your deck.
instance HasAbilities SoullessCadaver where
  getAbilities (SoullessCadaver a) = [mkAbility a 1 $ forced $ EnemyEngagedWindow When a.id]

instance RunMessage SoullessCadaver where
  runMessage msg e@(SoullessCadaver attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ [pid | Window _ (EnemyEngaged _ pid) <- ws] \pid -> do
        hand <- (.hand) <$> getPlayer pid
        for_ [c | c <- hand, printedCostOf c <= 2] (putFromHandOnBottomOfDeck pid)
      pure e
    _ -> SoullessCadaver <$> runMessage msg attrs
