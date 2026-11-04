module LordOfTheRings.Enemy.Cards.LostSoulOfLorien (lostSoulOfLorien) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Helpers.Character (getWillpower)
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Trait (Trait (Condition))

newtype LostSoulOfLorien = LostSoulOfLorien EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

lostSoulOfLorien :: EnemyCard LostSoulOfLorien
lostSoulOfLorien = enemy LostSoulOfLorien Cards.lostSoulOfLorien

-- | Defeated, it stays in play for its Forced effect to attach it. As a condition
-- attachment: "Limit 1 per hero. Attached hero's willpower is reduced to 0."
instance HasModifiersFor LostSoulOfLorien where
  getModifiersFor (LostSoulOfLorien a) = case a.placement of
    AttachedTo host -> do
      modifySelf a [GainTrait Condition]
      modified a host [WillpowerModifier (-99)]
    _ -> modifySelf a [StaysInPlayWhenDefeated]

{- | Forced: After Lost Soul of Lórien is destroyed, attach it to a hero with 1 or more
willpower, if able. The first player chooses the hero; with none able to take it, it is
discarded.
-}
instance RunMessage LostSoulOfLorien where
  runMessage msg (LostSoulOfLorien attrs) = case msg of
    CheckWindows ws
      | not (attached attrs)
      , notNull [() | Window After (EnemyDefeated eid) <- ws, eid == attrs.id] -> do
          souls <- select (EnemyWithCardCode $ toCardCode Cards.lostSoulOfLorien)
          taken <- (\es -> [h | e <- es, AttachedTo h <- [e.placement]]) <$> traverse getEnemy souls
          heroes <- filter ((`notElem` taken) . toTarget) <$> select AnyHero
          willing <- filterM (fmap (>= 1) . getWillpower . toTarget) heroes
          lead <- getLead
          if null willing
            then discard attrs
            else chooseOrRunOneM lead do
              questionLabeled "lostSoulOfLorien.attach"
              causedBy attrs
              targets willing \h -> do
                push $ DiscardShadowCards attrs.id
                placeEnemy attrs.id (AttachedTo $ toTarget h)
          LostSoulOfLorien <$> runMessage msg attrs
    PlaceEnemy eid (AttachedTo _) | eid == attrs.id -> LostSoulOfLorien <$> runMessage msg (attrs & tokensL .~ mempty)
    _ -> LostSoulOfLorien <$> runMessage msg attrs
   where
    attached a = case a.placement of
      AttachedTo _ -> True
      _ -> False
