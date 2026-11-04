module LordOfTheRings.Contract.Cards.CouncilOfTheWise (councilOfTheWise) where

import Data.Aeson.Types (parseMaybe)
import LordOfTheRings.Card (CardType (EventType))
import LordOfTheRings.Contract.CardDefs qualified as Cards
import LordOfTheRings.Contract.Import.Lifted

newtype CouncilOfTheWise = CouncilOfTheWise ContractAttrs
  deriving anyclass (IsContract, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

{- | You cannot include more than 1 copy of a card, by title, in your deck (checked as the
deck is chosen, 'LordOfTheRings.Decklist').
-}
councilOfTheWise :: ContractCard CouncilOfTheWise
councilOfTheWise = contract CouncilOfTheWise Cards.councilOfTheWise

-- | The options chosen this round, by step, kept in the meta until the round ends.
chosen :: ContractAttrs -> [Int]
chosen a = fromMaybe [] $ parseMaybe parseJSON a.meta

{- | Response: After you play an event from your hand, choose one: reduce your threat by 1,
draw 1 card, or add 1 resource to your hero's resource pool. You can only choose each
option once per round.
-}
instance HasAbilities CouncilOfTheWise where
  getAbilities (CouncilOfTheWise a) =
    [ mkAbility a 1 $ response $ PlayedCardFromHandWindow After a.owner (CardWithType EventType)
    | any (`notElem` chosen a) [1, 2, 3]
    ]

instance RunMessage CouncilOfTheWise where
  runMessage msg c@(CouncilOfTheWise attrs) = case msg of
    UseCardAbility pid (isSource attrs -> True) 1 _ -> do
      heroes <- select (HeroControlledBy $ PlayerWithId pid)
      let open n = n `notElem` chosen attrs
      chooseOneM pid do
        questionLabeled "councilOfTheWise.chooseOne"
        causedBy attrs
        when (open 1) $ labeled "councilOfTheWise.threat" $ doStep 1 msg
        when (open 2) $ labeled "councilOfTheWise.draw" $ doStep 2 msg
        when (open 3 && notNull heroes) $ labeled "councilOfTheWise.resource" $ doStep 3 msg
      pure c
    DoStep n (UseCardAbility pid (isSource attrs -> True) 1 _) -> do
      case n of
        1 -> lowerThreatBy attrs pid 1
        2 -> drawCards attrs pid 1
        _ -> do
          heroes <- select (HeroControlledBy $ PlayerWithId pid)
          chooseOrRunOneM pid do
            questionLabeled "councilOfTheWise.chooseHero"
            causedBy attrs
            targets heroes \h -> placeResources attrs h 1
      pure $ CouncilOfTheWise $ attrs {contractMeta = toJSON (n : chosen attrs)}
    EndRound -> pure $ CouncilOfTheWise $ attrs {contractMeta = Null}
    _ -> CouncilOfTheWise <$> runMessage msg attrs
