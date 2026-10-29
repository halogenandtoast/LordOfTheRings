module LordOfTheRings.Ally.Cards.HeraldOfAnorien (heraldOfAnorien) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Card (CardCost (StaticCost), CardType (AllyType), cdCost, toCardDef)
import LordOfTheRings.Keyword (Keyword (Doomed))
import LordOfTheRings.Placement (Placement (Unplaced))

newtype HeraldOfAnorien = HeraldOfAnorien AllyAttrs
  deriving anyclass IsAlly
  deriving newtype (Show, Eq, ToJSON, Entity)

heraldOfAnorien :: AllyCard HeraldOfAnorien
heraldOfAnorien = ally HeraldOfAnorien Cards.heraldOfAnorien

-- | You may give it doomed 2 when you play it from your hand. Asked while it is paid for.
instance HasModifiersFor HeraldOfAnorien where
  getModifiersFor (HeraldOfAnorien a) = when (a.placement == Unplaced) $ modified a (CardIdTarget a.allyCardId) [MayGainDoomed 2]

-- | If you do, it gains: "Response: After you play Herald of Anórien, choose a player. That player may put into
-- play 1 ally from his hand with a printed cost 2 or lower."
instance HasAbilities HeraldOfAnorien where
  getAbilities (HeraldOfAnorien a) =
    [ restricted a 1 (TargetHasModifier (CardIdTarget a.allyCardId) (GainKeyword (Doomed 2)))
        $ response
        $ PlayedFromHandWindow After (CardIdTarget a.allyCardId)
    ]

instance RunMessage HeraldOfAnorien where
  runMessage msg s@(HeraldOfAnorien a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      choosePlayerM pid a \p -> do
        hand <- (.hand) <$> getPlayer p
        let cheap c = c.cardType == AllyType && case cdCost (toCardDef c) of
              Just (StaticCost n) -> n <= 2
              _ -> False
        chooseOneM p do
          questionLabeled "heraldOfAnorien.chooseAlly"
          causedBy a
          targets (filter cheap hand) \c -> push $ PutAllyIntoPlay p c.id
          labeled "pass" (pure ())
      pure s
    _ -> HeraldOfAnorien <$> runMessage msg a
