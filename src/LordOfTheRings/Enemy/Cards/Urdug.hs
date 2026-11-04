module LordOfTheRings.Enemy.Cards.Urdug (urdug) where

import LordOfTheRings.Card (cdThreat, toCardDef)
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype Urdug = Urdug EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, Entity)

urdug :: EnemyCard Urdug
urdug = enemy Urdug Cards.urdug

-- | Immune to player card effects. Cannot leave the staging area, so no player is offered
-- to engage him: The Heroes' Defense has him considered engaged with each player instead.
instance HasModifiersFor Urdug where
  getModifiersFor (Urdug a) = modifySelf a [ImmuneToPlayerCardEffects, CannotLeaveStagingArea, CannotBeOptionallyEngagedBy AnyPlayer]

-- | Forced: When Urdug attacks, discard the top card of the encounter deck. He gets +X
-- attack for this attack, where X is the discarded card's printed threat.
-- When Urdug is destroyed, the players win the game.
instance HasAbilities Urdug where
  getAbilities (Urdug a) =
    [ mkAbility a 1 $ forced $ EnemyAttacksWindow When a.id
    , mkAbility a 2 $ forced $ ThisEnemyDefeatedWindow When a.id
    ]

instance RunMessage Urdug where
  runMessage msg e@(Urdug attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      top <- getTopOfEncounterDeck
      discardTopOfEncounterDeck attrs 1
      let x = maybe 0 (fromMaybe 0 . cdThreat . toCardDef) top
      when (x > 0) $ enemyGetsAttack attrs attrs.id x
      pure e
    UseCardAbility _ (isSource attrs -> True) 2 _ -> do
      winGame
      pure e
    _ -> Urdug <$> runMessage msg attrs
