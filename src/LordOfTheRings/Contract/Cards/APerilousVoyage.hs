module LordOfTheRings.Contract.Cards.APerilousVoyage (aPerilousVoyage) where

import LordOfTheRings.Card (Card (PlayerCard))
import LordOfTheRings.Contract.CardDefs qualified as Cards
import LordOfTheRings.Contract.Import.Lifted
import LordOfTheRings.Player.Runner (playableCardsFromDeckByEffect)

newtype APerilousVoyage = APerilousVoyage ContractAttrs
  deriving anyclass IsContract
  deriving newtype (Show, Eq, ToJSON, Entity)

aPerilousVoyage :: ContractCard APerilousVoyage
aPerilousVoyage = contract APerilousVoyage Cards.aPerilousVoyage

instance HasModifiersFor APerilousVoyage where
  getModifiersFor (APerilousVoyage a) = do
    modified
      a
      (PlayerTarget a.owner)
      [ if a.flipped then NonContractPlayerCardThreatReductionLimit 0 else PlayerCardThreatReductionLimit 0
      ]
    when a.flipped do
      heroes <- select $ HeroControlledBy $ PlayerWithId a.owner
      for_ heroes \hid -> modified a hid [DoesNotExhaustToCommit]

instance HasAbilities APerilousVoyage where
  getAbilities (APerilousVoyage a)
    | a.flipped = []
    | otherwise =
        [ restricted a 1 (PlayerExists $ PlayerWithId a.owner <> PlayerWithCardsInDeck 1)
            $ ActionAbility
            $ Costs [exhaustCost a, RaiseThreatCost 1]
        , mkAbility a 2 $ forced $ EliminatedByThreatWindow When a.owner
        ]

instance RunMessage APerilousVoyage where
  runMessage msg c@(APerilousVoyage attrs) = case msg of
    UseCardAbility pid (isSource attrs -> True) 1 _ -> do
      top <- take 2 . (.deck) <$> getPlayer pid
      chooseOneM pid do
        questionLabeled "aPerilousVoyage.draw"
        causedBy attrs
        showing top
        targets top \chosen -> do
          moveToTopOfDeck pid chosen
          drawCards attrs pid 1
          for_ (filter (/= chosen) top) $ moveToBottomOfDeck pid
      pure c
    UseCardAbility _ (isSource attrs -> True) 2 _ -> do
      cancelMatching (== EliminatePlayer attrs.owner)
      flipOver attrs
      pure c
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      -- The flip's Forced effect resolves with side B's threat restriction active.
      doStep 1 msg
      APerilousVoyage <$> runMessage msg attrs
    DoStep 1 (Flip (isTarget attrs -> True)) -> do
      lowerThreatBy attrs attrs.owner 10
      push CheckEliminations
      doStep 2 (Flip $ toTarget attrs)
      pure c
    DoStep 2 (Flip (isTarget attrs -> True)) -> do
      cards <- (.deck) <$> getPlayer attrs.owner
      playable <- playableCardsFromDeckByEffect attrs.owner 3
      chooseOneM attrs.owner do
        questionLabeled "aPerilousVoyage.search"
        causedBy attrs
        showing cards
        targets cards \chosen -> chooseOneM attrs.owner do
          questionLabeled "aPerilousVoyage.takeOrPlay"
          causedBy attrs
          labeled "aPerilousVoyage.take" $ takeFromDeck attrs.owner chosen
          for_ [pc | PlayerCard pc <- [chosen], chosen `elem` playable] \pc ->
            labeled "aPerilousVoyage.play" $ playFromDeck attrs attrs.owner pc 3
      shufflePlayerDeck attrs.owner
      pure c
    _ -> APerilousVoyage <$> runMessage msg attrs
