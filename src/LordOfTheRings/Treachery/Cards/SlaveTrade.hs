module LordOfTheRings.Treachery.Cards.SlaveTrade (slaveTrade) where

import LordOfTheRings.Card (cdCost, costAsResources, toCardDef)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Helpers.Character (getHitPoints)
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Trait (Trait (Raider))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype SlaveTrade = SlaveTrade TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

slaveTrade :: TreacheryCard SlaveTrade
slaveTrade = treachery SlaveTrade Cards.slaveTrade

-- | Place resources on a Raider enemy in play, the first player choosing which.
onARaider :: (ReverseQueue m, Sourceable source, Targetable source) => source -> Int -> m ()
onARaider source n = when (n > 0) do
  raiders <- select (EnemyWithTrait Raider)
  lead <- getLead
  chooseOrRunOneM lead do
    questionLabeled "slaveTrade.raider"
    causedBy source
    targets raiders \r -> placeResources source r n

-- | When Revealed: Each player deals 2 damage to an ally he or she controls. For each ally
-- destroyed by this effect, place resources on a Raider enemy in play equal to that ally's
-- resource cost.
-- Shadow: Discard an attachment you control. If attacking enemy is a Raider, add resources
-- to it equal to that attachment's printed cost.
instance RunMessage SlaveTrade where
  runMessage msg t@(SlaveTrade attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer \p -> do
        allies <- select (AllyControlledBy $ PlayerWithId p)
        chooseOrRunOneM p do
          questionLabeled "slaveTrade.ally"
          causedBy attrs
          targets allies \x -> do
            remaining <- (-) <$> getHitPoints (toTarget x) <*> getDamage (toTarget x)
            cost <- printedCostOf <$> getAlly x
            dealDamage attrs x 2
            when (remaining <= 2) $ onARaider attrs cost
      pure t
    ResolveShadowEffect cid eid pid _ | cid == attrs.cardId -> do
      attachments <- getsGame (toList . (.entities.attachments))
      raider <- selectAny (EnemyWithId eid <> EnemyWithTrait Raider)
      chooseOrRunOneM pid do
        questionLabeled "slaveTrade.discard"
        causedBy (CardIdTarget cid)
        targets [x | x <- attachments, (toAttrs x).controller == pid, AttachedTo _ <- [x.placement]] \x -> do
          discard x
          when raider $ placeResources (CardIdSource cid) eid $ maybe 0 costAsResources (cdCost $ toCardDef x)
      pure t
    _ -> SlaveTrade <$> runMessage msg attrs
