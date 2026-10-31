module LordOfTheRings.Enemy.Cards.PelargirRingleader (pelargirRingleader) where

import LordOfTheRings.Card (CardDef (..), CardType (EnemyType), toCardDef)
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Trait (Trait (Brigand))

newtype PelargirRingleader = PelargirRingleader EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

pelargirRingleader :: EnemyCard PelargirRingleader
pelargirRingleader = enemy PelargirRingleader Cards.pelargirRingleader

-- | Forced: When Pelargir Ringleader engages a player, return the topmost Brigand enemy in
-- the encounter discard pile to play, engaged with that player.
instance HasAbilities PelargirRingleader where
  getAbilities (PelargirRingleader a) = [mkAbility a 1 $ forced $ EnemyEngagedWindow After a.id]

-- | Shadow: Attacking enemy gets +2 attack.
instance RunMessage PelargirRingleader where
  runMessage msg e@(PelargirRingleader attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 windows -> do
      discards <- getEncounterDiscard
      let brigand c = cdCardType (toCardDef c) == EnemyType && Brigand `member` cdCardTraits (toCardDef c)
      for_ [pid | Window _ (EnemyEngaged _ pid) <- windows] \pid ->
        for_ (find brigand discards) \card -> do
          removeFromEncounterDiscard card
          putEnemyIntoPlayEngaged pid card
      pure e
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      enemyGetsAttack attrs eid 2
      pure e
    _ -> PelargirRingleader <$> runMessage msg attrs
