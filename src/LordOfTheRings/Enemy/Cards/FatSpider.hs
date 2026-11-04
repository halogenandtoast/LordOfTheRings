module LordOfTheRings.Enemy.Cards.FatSpider (fatSpider) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (Placement (EngagedWith))
import LordOfTheRings.Scenario.Scenarios.FliesAndSpiders.Helpers (givePoisonToCharacterOf)

newtype FatSpider = FatSpider EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Venom, on its card def.
fatSpider :: EnemyCard FatSpider
fatSpider = enemy FatSpider Cards.fatSpider

-- | This enemy gets +3 attack while engaged with a player who controls at least 1
-- poisoned character.
instance HasModifiersFor FatSpider where
  getModifiersFor (FatSpider a) = case a.placement of
    EngagedWith pid -> do
      poisoned <- notNull <$> selectCharacters (PoisonedHero <> HeroControlledBy (PlayerWithId pid)) (PoisonedAlly <> AllyControlledBy (PlayerWithId pid))
      when poisoned $ modifySelf a [AttackModifier 3]
    _ -> pure ()

-- | Shadow: Defending character gets 1 poison. (Give 2 poison to a character you control
-- if undefended.)
instance RunMessage FatSpider where
  runMessage msg e@(FatSpider attrs) = case msg of
    ResolveShadowEffect cid _ pid defenders | cid == attrs.cardId -> do
      if null defenders
        then givePoisonToCharacterOf attrs pid 2
        else for_ defenders \d -> givePoison d 1
      pure e
    _ -> FatSpider <$> runMessage msg attrs
