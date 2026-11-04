module LordOfTheRings.Enemy.Cards.CrazyCob (crazyCob) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.FliesAndSpiders.Helpers (poisonOn)

newtype CrazyCob = CrazyCob EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Venom, on its card def.
crazyCob :: EnemyCard CrazyCob
crazyCob = enemy CrazyCob Cards.crazyCob

{- | When Revealed: This enemy attacks the character with the most poison attached. With
none poisoned, or a tie, the first player chooses among them (FAQ, Crazy Cob). The attack
is against the character's controller, who may declare a defender; undefended, its damage
must go to that character (FAQ 1.41).
Shadow: Defending character gets 1 poison.
-}
instance RunMessage CrazyCob where
  runMessage msg e@(CrazyCob attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      characters <- selectCharacters (HeroControlledBy AnyPlayer) (AllyControlledBy AnyPlayer)
      counts <- for characters \c -> (c,) <$> poisonOn c
      let most = foldr (max . snd) 0 counts
      lead <- getLead
      chooseOrRunOneM lead do
        questionLabeled "crazyCob.attack"
        causedBy attrs
        targets [c | (c, n) <- counts, n == most] \c -> do
          controller <- characterController c
          for_ controller \pid -> do
            attackModifier attrs attrs.id (UndefendedDamageTo c)
            immediateAttack pid attrs.id
      pure e
    ResolveShadowEffect cid _ _ defenders | cid == attrs.cardId -> do
      for_ defenders \d -> givePoison d 1
      pure e
    _ -> CrazyCob <$> runMessage msg attrs
