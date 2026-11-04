module LordOfTheRings.Enemy.Cards.ChampionOfRhun (championOfRhun) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Helpers.ControlledLocation (controlledLocations, returnToStaging)
import LordOfTheRings.Placement (Placement (EngagedWith))

newtype ChampionOfRhun = ChampionOfRhun EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

championOfRhun :: EnemyCard ChampionOfRhun
championOfRhun = enemy ChampionOfRhun Cards.championOfRhun

-- | Forced: After Champion of Rhûn engages you, either return a location you control to the
-- staging area, or Champion of Rhûn makes an attack. Controlling none, he attacks.
instance HasAbilities ChampionOfRhun where
  getAbilities (ChampionOfRhun a) = [mkAbility a 1 $ forced $ EnemyEngagedWindow After a.id]

-- | Shadow: Deal 1 damage to defending character.
instance RunMessage ChampionOfRhun where
  runMessage msg e@(ChampionOfRhun attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      for_ [pid | EngagedWith pid <- [attrs.placement]] \pid -> do
        locations <- controlledLocations pid
        chooseOrRunOneM pid do
          questionLabeled "championOfRhun.choose"
          causedBy attrs
          targets locations returnToStaging
          labeled "championOfRhun.attack" $ immediateAttack pid attrs.id
      pure e
    ResolveShadowEffect cid _ _ defenders | cid == attrs.cardId -> do
      for_ defenders \d -> dealDamage attrs d 1
      pure e
    _ -> ChampionOfRhun <$> runMessage msg attrs
