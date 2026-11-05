module LordOfTheRings.Enemy.Cards.HuntingHawk (huntingHawk) where

import LordOfTheRings.Ally.Types (AllyAttrs (..))
import LordOfTheRings.Card (CardType (AllyType))
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Scenario.Scenarios.TheRingGoesSouth.Helpers (damageAnActiveLocation)

newtype HuntingHawk = HuntingHawk EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, Entity)

huntingHawk :: EnemyCard HuntingHawk
huntingHawk = enemy HuntingHawk Cards.huntingHawk

instance HasModifiersFor HuntingHawk where
  getModifiersFor (HuntingHawk a) = modifySelf a [CannotHaveAttachments, CannotBeOptionallyEngagedBy AnyPlayer]

instance HasAbilities HuntingHawk where
  getAbilities (HuntingHawk a) =
    [ mkAbility a 1 $ forced $ CardPlayedWindow After (CardWithType AllyType) | a.placement == StagingArea
    ]

instance RunMessage HuntingHawk where
  runMessage msg e@(HuntingHawk a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 ws -> do
      for_ [(pid, card) | Window _ (PlayedCard pid card) <- ws] $ \(pid, card) -> do
        allies <- getsGame (filter ((== card.id) . allyCardId . toAttrs) . toList . (.entities.allies))
        exhaustible <-
          filterM
            (fmap (notElem CannotExhaust) . getModifiers)
            (filter (not . allyExhausted . toAttrs) allies)
        actives <- select IsActiveLocation
        chooseOrRunOneM pid do
          questionLabeled "huntingHawk.choose"
          causedBy a
          when (notNull exhaustible || null actives)
            $ labeled "huntingHawk.exhaust"
            $ traverse_ exhaust (take 1 exhaustible)
          when (notNull actives || null exhaustible)
            $ labeled "huntingHawk.damage"
            $ damageAnActiveLocation a 1
      pure e
    _ -> HuntingHawk <$> runMessage msg a
